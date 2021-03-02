library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(igraph)
library(Rtsne)
library(visNetwork)
library(qualtRics)
library(RColorBrewer)

options(shiny.reactlog=TRUE)

############################################################
##### CONSTANTS

kColorButtonDefault <- 'None'
kNetworkAdvice <- 'Advice'
kNetworkSupport <- 'Support'
kConsent <- 'I agree'
kNodeSizeBase <- 18
kNodeSizeReduced <- 12

############################################################
##### utility functions

rescale <- function(x, minimum = 0, maximum = 1) { minimum + (maximum - minimum) * (x-min(x))/(max(x)-min(x)) }

color.pal <- function(pal = 'Greens', na.value = 0.5) {
    # replace missing with default value? with gray?
    function(x) {
        x[is.na(x)] <- na.value
        cr <- colorRamp(brewer.pal(9, pal), space='Lab')(x)
        #cr[is.na(cr)] <- 170
        rgb(cr[,1], cr[,2], cr[,3], maxColorValue = 255)
    }
}

############################################################
##### network setup

# load qualtrics survey file (error checking, various formats) and return as dataframee
load_qualtrics_file <- function(file_qualtrics) {
    # TODO: is filetype checking robust??
    #   can maybe wrap everything in a tryCatch
    suppressMessages({
        fHandle <- file_qualtrics$datapath
        if(file_qualtrics$type == "application/zip") { fHandle <- unzip(fHandle) }
        df.qualtrics <- qualtRics::read_survey(fHandle)
    })
    df.qualtrics
}

# convert survey dataframe into a comprehensive igraph network
qualtrics_to_igraph <- function(df.qualtrics) {
    df.qualtrics <- df.qualtrics %>% mutate(ego = NodeID)
    # extract and longen network data
    df.net <- df.qualtrics %>%
        pivot_longer(cols = starts_with('network_'),
                     names_prefix = 'network_',
                     names_to = 'alter') %>%
        mutate(alter = as.numeric(alter))
    # extract networks
    df.net$advice <- grepl(kNetworkAdvice, df.net$value)
    df.net$support <- grepl(kNetworkSupport, df.net$value)
    # drop self-ties
    df.net <- df.net %>%
        filter(ego != alter)
    # TODO: extract node characteristics
    df.nodes <- data.frame(id = unique(df.net$alter)) %>%
        left_join(df.qualtrics, by = c('id' = 'NodeID')) %>%
        select(id, FullName, consent)
    # add names, fixing missing nodes
    df.nodes <- df.nodes %>%
        mutate(maybeLabel = ifelse((consent == kConsent) & !(is.na(consent)), FullName, '???'),
               bConsent = (consent == kConsent) & !(is.na(consent))) %>%
        select(id, maybeLabel, bConsent) %>%
        rename(consent = bConsent)
    # build igraph
    ntwk <- df.net %>%
        select(ego, alter, advice, support) %>%
        graph_from_data_frame(directed = TRUE, vertices = df.nodes)
    E(ntwk)$width <- 1
    ntwk
}

# extract directional and valenced networks from master igraph
build_network_directions <- function(g, slug = 'g') {
    # "directed" networks (preserving weight)
    g.d <- as.undirected(g, 'collapse', edge.attr.comb = 'sum') %>%
        add_centralities(g)
    # undirected networks
    g.mc <- as.undirected(g, 'collapse') %>%
        add_centralities(g)
    g.mm <- as.undirected(g, 'mutual') %>%
        add_centralities(g)
    # reset widths
    E(g.d)$width <- 2 * E(g.d)$width - 1
    E(g.mc)$width <- 1
    E(g.mm)$width <- 1
    # export
    res <- list(g.d, g.mc, g.mm)
    names(res) <- paste0(slug, c('.directed', '.any', '.mutual'))
    res
}

add_centralities <- function(g, g0) {
    # get centralities
    # TODO: add `constraint` (Burt's constraint)
    V(g)$centrality.outdegree <- degree(g0, mode = 'out')
    V(g)$centrality.indegree <- degree(g0, mode = 'in')
    V(g)$centrality.close <- closeness(g)
    V(g)$centrality.between <- betweenness(g)
    V(g)$centrality.eigen <- eigen_centrality(g)$vector
    V(g)$centrality.cluster <- transitivity(g, type = 'barrat', isolates = 'zero')
    g
}

# convert base network into a complete network with multiple types of edge weight
build_edge_network <- function(g) {
    ge <- as.undirected(g, mode = 'collapse', edge.attr.comb = sum)
    # create ids
    E(ge)$id <- 1:length(E(ge))
    # advice network
    E(ge)$advice.directed <- E(ge)$advice
    E(ge)$advice.any      <- as.numeric(E(ge)$advice > 0)
    E(ge)$advice.mutual   <- as.numeric(E(ge)$advice == 2)
    # support network
    E(ge)$support.directed <- E(ge)$support
    E(ge)$support.any      <- as.numeric(E(ge)$support > 0)
    E(ge)$support.mutual   <- as.numeric(E(ge)$support == 2)
    # done
    ge
}

# set node color by centrality metric
set_node_color <- function(metric, g) {
    cent <- switch(metric,
                   eigen=rescale(g$centrality.eigen),
                   between=rescale(g$centrality.between),
                   close=rescale(g$centrality.close),
                   indegree=rescale(g$centrality.indegree),
                   outdegree=rescale(g$centrality.outdegree),
                   cluster=rescale(g$centrality.cluster),
                   0)
    # restrict observed range to [0.5, 1]
    cent <- 0.5 + 0.5 * cent
    # set colors
    g$color.background <- color.pal()(cent)
    #g$color.border <- color.pal()(cent)
    g$color.highlight.background <- color.pal()(cent)
    g$color.highlight.border <- color.pal()(cent)
    g
}

# set node size by anonymity consent
set_node_size <- function(consenters, g) {
    if('highlight' %in% consenters) {
        g$size <- ifelse(g$consent, kNodeSizeBase, kNodeSizeReduced)
    } else {
        g$size <- kNodeSizeBase
    }
    g
}

# set edge charactertistics by network type & direction
set_edge_physics <- function(ge, networkType, networkDirection) {
    edges <- edge.attributes(ge)[[paste0(networkType, networkDirection)]]
    E(ge)$physics <- edges > 0
    E(ge)$hidden <- edges == 0
    E(ge)$width <- 2 * edges - 1
    ge
}


############################################################
##### network export

report.factory <- function(file, ga, gs, node) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    params <- list(ga = ga, gs = gs, node = node)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
}

############################################################
##### SERVER

server <- function(input, output, session) {
    qualtricsNetworks <- reactive({
        # load network
        req(input$inFile)
        df.qualtrics <- load_qualtrics_file(input$inFile)
        # convert to igraph
        g <- qualtrics_to_igraph(df.qualtrics)
        # TODO: build centralities and layouts here?
        # build networks: full, advice, support
        res.full <- subgraph.edges(g, which(E(g)$advice | E(g)$support), delete.vertices = FALSE) %>%
            build_network_directions('full')
        res.advice <- subgraph.edges(g, which(E(g)$advice), delete.vertices = FALSE) %>%
            build_network_directions('advice')
        res.support <- subgraph.edges(g, which(E(g)$support), delete.vertices = FALSE) %>%
            build_network_directions('support')
        # build full edges network
        ge <- build_edge_network(g)
        # export networks
        c(res.full, res.advice, res.support, list(edges = ge))
    })

    ntwk <- reactive({
        qntwk <- qualtricsNetworks()
        # reset radio buttons
        #updateRadioGroupButtons(session, 'colorMetric', selected = kColorButtonDefault)
        #updateRadioGroupButtons(session, 'colorMetric', selected = kColorButtonDefault)
        netType <- input$networkTie
        netDir <- input$networkDirection
        # extract and augment correct network
        g <- qntwk[[paste0(netType, netDir)]] %>%
            #add_layout_(with_fr()) %>%
            toVisNetworkData(idToLabel = FALSE)
        print(qntwk[["edges"]])
        ge <- set_edge_physics(qntwk[["edges"]], netType, netDir) %>%
            toVisNetworkData(idToLabel = FALSE)
        #list(nodes = g, edges = ge)
        list(nodes = g$nodes, edges = ge$edges)
    })

    output$network <- renderVisNetwork({
        qntwk <- qualtricsNetworks()
        g <- isolate(ntwk())
        #g <- ntwk()
        #g$nodes <- set_node_color(kColorButtonDefault, g$nodes)
        g$nodes <- set_node_color(isolate(input$colorMetric), g$nodes)
        g$nodes <- set_node_size(isolate(input$consenters), g$nodes)
        visNetwork(nodes = g$nodes, edges = g$edges, width = "100%", height = "100%") %>%
            visNodes(size = kNodeSizeBase,
                     borderWidth = 2,
                     font = list(strokeWidth = 4),
                     color = list(border = "white", highlight = list(border = "white"))) %>%
            visEdges(color = "#aaa") %>%
            #visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
            visEvents(selectNode = 'function(properties) { Shiny.setInputValue("selectedNodes", properties.nodes); console.log(properties) }') %>%
            visEvents(deselectNode = 'function(properties) { Shiny.setInputValue("selectedNodes", properties.nodes); console.log(properties) }') %>%
            #visEvents(type = 'once', afterDrawing = 'function(properties) { Shiny.setInputValue("actionRedraw", true, {"priority":"event"}); console.log("redraw"); console.log(properties) }') %>%
            #visPhysics(enabled = FALSE) %>%
            visExport()
    })

    # update network on mode/direction change
    observe({
        g <- ntwk()
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes) %>%
            visUpdateEdges(g$edges)
    })

    # display node's name: if it is selected and reveal name checkbox is enabled
    observe({
        g <- ntwk()
        if(!is.null(input$selectedNodes) && ('reveal' %in% input$consenters)) {
            g$nodes$label <- ifelse(g$nodes$id == input$selectedNodes, g$nodes$maybeLabel, '')
        } else {
            g$nodes$label <- ''
        }
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
    })

    # update colors by centrality
    observe({
        #input$actionRedraw
        g <- ntwk()
        g$nodes <- set_node_color(input$colorMetric, g$nodes)
        # update nodes
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
    })

    # highlight revealable people with size
    observe({
        #input$actionRedraw
        g <- ntwk()
        g$nodes <- set_node_size(input$consenters, g$nodes)
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
    })

    observeEvent(input$physics, {
        visNetworkProxy("network") %>%
            visStorePositions()
    })

    #output$selcol <- renderText({ input$inFile$type })

    output$selcoltable <- renderTable({ ntwk()$nodes })

    output$selcoltable2 <- renderTable({ ntwk()$edges })

    output$downloadUI <- renderUI({
        req(qualtricsNetworks())
        tagList(
            p('Download network data: '),
            downloadButton("downloadData", label = "Download")
        )
    })

    output$downloadData <- downloadHandler(
        filename = "section-network.graphml",
        content = function(file) {
            igraph::write_graph(qualtricsNetworks()$full.directed, file, format = "graphml")
        }
    )

    output$report <- downloadHandler(
        filename = "reports.zip",
        #filename = "reports.html",
        content = function(file) {
            ns <- qualtricsNetworks()
            nvs <- length(V(ns$full.directed))
            #nvs <- min(nvs, 3)
            # extract advice and support networks
            ga <- ns$advice.directed
            ga <- add_layout_(ga, with_dh())
            gs <- ns$support.directed
            gs <- add_layout_(gs, with_dh())
            # create file paths
            paths <- file.path(tempdir(), sprintf('report-%04d.html', 1:nvs))
            withProgress(message = 'Generating reports',
                         value = 0,
                         detail = paste(0, '/', nvs), 
                {
                    for(i in 1:nvs) {
                        report.factory(paths[[i]], ga = ga, gs = gs, node = i)
                        #print(ns$full.directed)
                        incProgress(1/nvs, detail = paste(i, "/", nvs))
                    }
                })
            zip::zip(file, files = paths, mode = 'cherry-pick')
            #file.copy(paths[[1]], file)
            #file.copy(paths[[27]], file)
        }
    )
}

############################################################
##### UI

ui <- fluidPage(
    #shinyjs::useShinyjs(),
    sidebarLayout(
        sidebarPanel(
            titlePanel('Section Network Map'),
            fileInput("inFile", "Upload Qualtrics file (zip/csv)", accept = c(".zip", ".csv")),
            uiOutput("downloadUI"),
            hr(),
            radioGroupButtons('networkTie',
                              'Select network',
                              justified = TRUE,
                              choiceNames = c('Advice', 'Support'),
                              choiceValues = c('advice', 'support'),
                              selected = 'advice'),
            radioGroupButtons('networkDirection',
                              'Select network direction',
                              justified = TRUE,
                              choiceNames = c('Directed', 'Mutual', 'Any'),
                              choiceValues = c('.directed', '.mutual', '.any'),
                              selected = '.directed'),
            radioGroupButtons('colorMetric',
                              'Color by',
                              choiceNames = c(kColorButtonDefault, 'Out-Degree', 'In-Degree', 'Closeness', 'Betweenness', 'Eigenvector', 'Cluster'),
                              choiceValues = c(kColorButtonDefault, 'outdegree', 'indegree', 'close', 'between', 'eigen', 'cluster'),
                              selected = kColorButtonDefault),
            checkboxGroupButtons('consenters',
                                 'Reveal names:',
                                 justified = TRUE,
                                 #status = 'info',
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 choiceNames = c('Highlight consenters', 'Reveal names'),
                                 choiceValues = c('highlight', 'reveal')),
            hr(),
            tagList(
                p('Generate reports'),
                downloadButton("report", label = "Download"),
            ),
            #hr(),
            #actionButton("physics", label = "Physics"),
            NULL
        ),

        mainPanel(
            visNetworkOutput("network", height="600px"),
            #tableOutput('selcoltable'),
            #tableOutput('selcoltable2'),
            NULL
        )
    )
)

############################################################
##### APP

shinyApp(ui = ui, server = server)
