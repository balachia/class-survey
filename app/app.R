library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(igraph)
#library(Rtsne)
library(visNetwork)
#library(qualtRics)
library(RColorBrewer)
library(networkSurveyBackend)

options(shiny.reactlog=TRUE)

############################################################
##### CONSTANTS

kQuestionSudo <- 'pseudonym'
kQuestionConsent <- 'consent'
kQuestionNetwork <- 'network'
kColorButtonDefault <- 'None'
kNetworkAdvice <- 'Advice'
kNetworkSupport <- 'Support'
kConsent <- 'I agree'
kNodeSizeBase <- 20
kNodeSizeReduced <- 0.5
kCentralityMinColor <- 0.2
kLabelFontSize <- 24

############################################################
##### utility functions

rescale <- function(x, minimum = 0, maximum = 1, na.rm = TRUE) { minimum + (maximum - minimum) * (x-min(x, na.rm = na.rm))/(max(x, na.rm = na.rm)-min(x, na.rm = na.rm)) }

color.pal <- function(pal = 'YlGn', na.value = 0.5) {
    # replace missing with default value? with gray?
    function(x) {
        x[is.na(x)] <- na.value
        cr <- colorRamp(brewer.pal(9, pal), space='Lab')(x)
        #cr[is.na(cr)] <- 170
        rgb(cr[,1], cr[,2], cr[,3], maxColorValue = 255)
    }
}

color.pal.qual <- function() {
    #brewer.pal(12, 'Paired')
    #brewer.pal(8, 'Dark2')
    brewer.pal(8, 'Set1')
}

############################################################
##### network setup

# load qualtrics survey file (error checking, various formats) and return as dataframee
#load_qualtrics_file <- function(file_qualtrics) {
#    # TODO: is filetype checking robust??
#    #   can maybe wrap everything in a tryCatch
#    suppressMessages({
#        fHandle <- file_qualtrics$datapath
#        if(file_qualtrics$type == "application/zip") { fHandle <- unzip(fHandle) }
#        df.qualtrics <- qualtRics::read_survey(fHandle)
#    })
#    df.qualtrics
#}

# convert survey dataframe into a comprehensive igraph network
#qualtrics_to_igraph <- function(df.qualtrics) {
#    df.qualtrics <- df.qualtrics %>% mutate(ego = NodeID)
#    # extract and longen network data
#    net.prefix <- paste0(kQuestionNetwork, '_')
#    df.net <- df.qualtrics %>%
#        pivot_longer(cols = starts_with(net.prefix),
#                     names_prefix = net.prefix,
#                     names_to = 'alter') %>%
#        mutate(alter = as.numeric(alter))
#    # extract networks
#    df.net$advice <- grepl(kNetworkAdvice, df.net$value)
#    df.net$support <- grepl(kNetworkSupport, df.net$value)
#    # drop self-ties
#    df.net <- df.net %>%
#        filter(ego != alter)
#    # fix pseudonyms
#    if(!(kQuestionSudo %in% colnames(df.qualtrics))) {
#        df.qualtrics <- df.qualtrics %>%
#            mutate("{kQuestionSudo}" := "")
#    }
#    # TODO: extract node characteristics
#    df.nodes <- data.frame(id = unique(df.net$alter)) %>%
#        left_join(df.qualtrics, by = c('id' = 'NodeID')) %>%
#        rename(consent = all_of(kQuestionConsent), sudo = all_of(kQuestionSudo)) %>%
#        select(id, FullName, consent, sudo)
#    # add names, fixing missing nodes
#    df.nodes <- df.nodes %>%
#        mutate(maybeLabel = ifelse((consent == kConsent) & !(is.na(consent)), FullName, '???'),
#               bConsent = (consent == kConsent) & !(is.na(consent))) %>%
#        select(id, maybeLabel, bConsent, sudo) %>%
#        rename(consent = bConsent)
#    # build igraph
#    ntwk <- df.net %>%
#        select(ego, alter, advice, support) %>%
#        graph_from_data_frame(directed = TRUE, vertices = df.nodes)
#    E(ntwk)$width <- 1
#    ntwk
#}

## extract directional and valenced networks from master igraph
#build_network_directions <- function(g, slug = 'g') {
#    # "directed" networks (preserving weight)
#    g.d <- as.undirected(g, 'collapse', edge.attr.comb = 'sum') %>%
#        add_centralities(g)
#    # undirected networks
#    g.mc <- as.undirected(g, 'collapse') %>%
#        add_centralities(g)
#    g.mm <- as.undirected(g, 'mutual') %>%
#        add_centralities(g)
#    # reset widths
#    E(g.d)$width <- 2 * E(g.d)$width - 1
#    E(g.mc)$width <- 1
#    E(g.mm)$width <- 1
#    # export
#    res <- list(g.d, g.mc, g.mm)
#    names(res) <- paste0(slug, c('.directed', '.any', '.mutual'))
#    res
#}

#add_centralities <- function(g, g0) {
#    # get centralities
#    # TODO: add `constraint` (Burt's constraint)
#    V(g)$centrality.outdegree <- degree(g0, mode = 'out')
#    V(g)$centrality.indegree <- degree(g0, mode = 'in')
#    #suppressWarnings({
#    #    V(g)$centrality.close <- closeness(g)
#    #})
#    #V(g)$centrality.between <- betweenness(g)
#    #V(g)$centrality.eigen <- eigen_centrality(g)$vector
#    #V(g)$centrality.cluster <- transitivity(g, type = 'barrat', isolates = 'zero')
#    suppressWarnings({
#        V(g)$centrality.close <- closeness(g0, mode = 'in')
#    })
#    V(g)$centrality.between <- betweenness(g0)
#    V(g)$centrality.eigen <- eigen_centrality(g0, directed = TRUE)$vector
#    V(g)$centrality.cluster <- transitivity(g0, type = 'barrat', isolates = 'zero')
#    g
#}

## convert base network into a complete network with multiple types of edge weight
#build_edge_network <- function(g) {
#    ge <- as.undirected(g, mode = 'collapse', edge.attr.comb = sum)
#    # create ids
#    E(ge)$id <- 1:length(E(ge))
#    # advice network
#    E(ge)$advice.directed <- E(ge)$advice
#    E(ge)$advice.any      <- as.numeric(E(ge)$advice > 0)
#    E(ge)$advice.mutual   <- as.numeric(E(ge)$advice == 2)
#    # support network
#    E(ge)$support.directed <- E(ge)$support
#    E(ge)$support.any      <- as.numeric(E(ge)$support > 0)
#    E(ge)$support.mutual   <- as.numeric(E(ge)$support == 2)
#    # done
#    ge
#}

# set node color by centrality metric
set_node_color <- function(metric, community.no, g) {
    # qualitative vs continuous metric
    if(metric %in% c('community')) {
        cent <- g[[paste0('community.', community.no)]]
        pal <- color.pal.qual()
        # set colors
        g$color.background <- pal[cent]
        g$color.highlight.background <- pal[cent]
        g$color.highlight.border <- pal[cent]
    } else {
        cent <- switch(metric,
                       eigen=rescale(g$centrality.eigen),
                       between=rescale(g$centrality.between),
                       close=rescale(g$centrality.close),
                       indegree=rescale(g$centrality.indegree),
                       outdegree=rescale(g$centrality.outdegree),
                       cluster=rescale(g$centrality.cluster),
                       0.5)
        # restrict observed range to [0.5, 1]
        cent <- kCentralityMinColor + (1 - kCentralityMinColor) * cent
        # set colors
        g$color.background <- color.pal()(cent)
        #g$color.border <- color.pal()(cent)
        g$color.highlight.background <- color.pal()(cent)
        g$color.highlight.border <- color.pal()(cent)
    }
    g
}

# set node size by anonymity consent
set_node_size <- function(consenters, g, nodeSizeBase = kNodeSizeBase) {
    if('highlight' %in% consenters) {
        g$size <- nodeSizeBase * ifelse(g$consent, 1, kNodeSizeReduced)
    } else {
        g$size <- nodeSizeBase
    }
    g
}

# set edge charactertistics by network type & direction
set_edge_physics <- function(ge, networkType, networkDirection, width.weight = 1) {
    edges <- edge.attributes(ge)[[paste0(networkType, networkDirection)]]
    directed <- edge.attributes(ge)[['directed']]
    E(ge)$physics <- edges > 0
    E(ge)$hidden <- edges == 0
    E(ge)$width <- width.weight * ((edges > 0) + 2 * (edges > 1))
    E(ge)$arrows <- ifelse(directed, 'to', '')
    #E(ge)$arrows <- 'to'
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
    #qualtricsNetworks <- reactive({
    #    # load network
    #    req(input$inFile)
    #    df.qualtrics <- load_qualtrics_file(input$inFile)
    #    # convert to igraph
    #    g <- qualtrics_to_igraph(df.qualtrics)
    #    # TODO: build centralities and layouts here?
    #    # build networks: full, advice, support
    #    res.full <- subgraph.edges(g, which(E(g)$advice | E(g)$support), delete.vertices = FALSE) %>%
    #        build_network_directions('full')
    #    res.advice <- subgraph.edges(g, which(E(g)$advice), delete.vertices = FALSE) %>%
    #        build_network_directions('advice')
    #    res.support <- subgraph.edges(g, which(E(g)$support), delete.vertices = FALSE) %>%
    #        build_network_directions('support')
    #    # build full edges network
    #    ge <- build_edge_network(g)
    #    # export networks
    #    c(res.full, res.advice, res.support, list(edges = ge))
    #})

    qualtricsNetworks <- reactive({
        # load network
        req(input$inFile)
        df.qualtrics <- networkSurveyBackend::load_qualtrics_file(input$inFile)
        # convert to igraph
        g <- networkSurveyBackend::qualtrics_to_igraph(df.qualtrics,
                qidNetwork = kQuestionNetwork, qidSudo = kQuestionSudo, qidConsent = kQuestionConsent,
                ansAdvice = kNetworkAdvice, ansSupport = kNetworkSupport, ansConsent = kConsent)
        # TODO: build centralities and layouts here?
        # build networks: full, advice, support
        res.full <- subgraph.edges(g, which(E(g)$advice | E(g)$support), delete.vertices = FALSE) %>%
            networkSurveyBackend::build_network_directions('full')
        res.advice <- subgraph.edges(g, which(E(g)$advice), delete.vertices = FALSE) %>%
            networkSurveyBackend::build_network_directions('advice')
        res.support <- subgraph.edges(g, which(E(g)$support), delete.vertices = FALSE) %>%
            networkSurveyBackend::build_network_directions('support')
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
        #print(qntwk[["edges"]])
        ge <- set_edge_physics(qntwk[["edges"]], netType, netDir, derivedSizeBases()$edge) %>%
            toVisNetworkData(idToLabel = FALSE)
        #list(nodes = g, edges = ge)
        list(nodes = g$nodes, edges = ge$edges)
    })

    output$network <- renderVisNetwork({
        qntwk <- qualtricsNetworks()
        g <- isolate(ntwk())
        #g <- ntwk()
        #g$nodes <- set_node_color(kColorButtonDefault, g$nodes)
        g$nodes <- set_node_color(isolate(input$colorMetric), isolate(input$communityNumber), g$nodes)
        g$nodes <- set_node_size(isolate(input$consenters), g$nodes)
        visNetwork(nodes = g$nodes, edges = g$edges, width = "100%", height = "100%") %>%
            visNodes(size = kNodeSizeBase,
                     borderWidth = 2,
                     font = list(strokeWidth = 5, size = kLabelFontSize),
                     color = list(border = "white", highlight = list(border = "white"))) %>%
            visEdges(color = "#aaa") %>%
            #visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
            visEvents(selectNode = 'function(properties) { Shiny.setInputValue("selectedNodes", properties.nodes); console.log(properties) }') %>%
            visEvents(deselectNode = 'function(properties) { Shiny.setInputValue("selectedNodes", properties.nodes); console.log(properties) }') %>%
            visEvents(stabilized = 'function(properties) { Shiny.setInputValue("actionStabilized", true, {"priority":"event"}); console.log("first stabilized"); console.log(properties) }') %>%
            visPhysics(solver = "barnesHut",
                       maxVelocity = 16,
                       barnesHut = list(gravitationalConstant = -10000, centralGravity = 3, damping = 0.09),
                       stabilization = list(iterations = 1000, fit = TRUE)) %>%
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
        # set all label font sizes
        g$nodes$font.size <- derivedSizeBases()$label
        # show pseudonym or real name
        if(!is.null(input$pseudonyms) && ('sudos' %in% input$pseudonyms)) {
            g$nodes$label <- g$nodes$sudo
        } else if(!is.null(input$selectedNodes) && ('reveal' %in% input$consenters)) {
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
        g$nodes <- set_node_color(input$colorMetric, input$communityNumber, g$nodes)
        # update nodes
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
    })

    # highlight revealable people with size
    observe({
        #input$actionRedraw
        g <- ntwk()
        g$nodes <- set_node_size(input$consenters, g$nodes, derivedSizeBases()$node)
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
    })

    #observeEvent(input$physics, {
    #    visNetworkProxy("network") %>%
    #        visPhysics(enabled = TRUE)
    #})

    # set various sizes after determining initial graph diameter
    observeEvent(input$actionStabilized, {
        visNetworkProxy("network") %>%
            #visFit(animation = list(duration = 2000)) %>%
            visGetPositions()
    })

    #observeEvent(ntwk(), {
    #    visNetworkProxy("network") %>%
    #        visGetPositions()
    #})

    #derivedNodeSizeBase <- reactive({
    #    span <- function(x) { max(x) - min(x) }
    #    pos <- (input$network_positions)
    #    if(!is.null(pos)) {
    #        span.x <- map_dbl(pos, 'x') %>%
    #            span()
    #        span.y <- map_dbl(pos, 'x') %>%
    #            span()
    #        kNodeSizeBase * (max(span.x, span.y) / 1000)
    #    } else {
    #        kNodeSizeBase
    #    }
    #})

    derivedSizeBases <- reactive({
        span <- function(x) { max(x) - min(x) }
        pos <- (input$network_positions)
        if(!is.null(pos)) {
            span.x <- map_dbl(pos, 'x') %>%
                span()
            span.y <- map_dbl(pos, 'x') %>%
                span()
            nspan <- max(span.x, span.y) / 1000
            list(node = kNodeSizeBase * nspan,
                 label = kLabelFontSize * nspan,
                 edge = nspan)
        } else {
            list(node = kNodeSizeBase,
                 label = kLabelFontSize,
                 edge = 1)
        }
    })


    #observe({
    #    visNetworkProxy("network") %>%
    #        visPhysics(solver = "barnesHut",
    #                   #timestep = 0.1,
    #                   maxVelocity = input$speed,
    #                   barnesHut = list(gravitationalConstant = input$gravK, centralGravity = input$cgrav * input$gravK * (-0.3/2000), damping = 0.09),
    #                   stabilization = list(iterations = 1000, fit = TRUE))
    #})

    #output$selcol <- renderText({ input$inFile$type })

    #output$selcoltable <- renderTable({ ntwk()$nodes })
    #output$selcoltable2 <- renderTable({ ntwk()$edges })

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
        }
    )
}

############################################################
##### UI

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            titlePanel('Section Network Map'),
            fileInput("inFile", "Upload Qualtrics file (zip/csv)", accept = c(".zip", ".csv")),
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
                              choiceNames = c('Any', 'Mutual', 'Directed'),
                              choiceValues = c('.any', '.mutual', '.directed'),
                              selected = '.any'),
            radioGroupButtons('colorMetric',
                              'Color by',
                              choiceNames = c(kColorButtonDefault, 'Out-Degree', 'In-Degree', 'Closeness', 'Betweenness', 'Eigenvector', 'Cluster', 'Community'),
                              choiceValues = c(kColorButtonDefault, 'outdegree', 'indegree', 'close', 'between', 'eigen', 'cluster', 'community'),
                              selected = kColorButtonDefault),
            sliderInput('communityNumber',
                        'Number of communities',
                        min = 1, max = 8, value = 1, step = 1, round = TRUE, ticks = FALSE),
            checkboxGroupButtons('consenters',
                                 'Real names:',
                                 justified = TRUE,
                                 #status = 'info',
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 choiceNames = c('Highlight consenters', 'Reveal selected'),
                                 choiceValues = c('highlight', 'reveal')),
            checkboxGroupButtons('pseudonyms',
                                 'Pseudonyms:',
                                 justified = TRUE,
                                 #status = 'info',
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 choiceNames = c('Show pseudonyms'),
                                 choiceValues = c('sudos')),
            hr(),
            uiOutput("downloadUI"),
            p('Generate reports at',
                a('https://balachia.shinyapps.io/ob2-reports',
                  href = 'https://balachia.shinyapps.io/ob2-reports',
                  target = '_blank')),
            #hr(),
            #actionButton("physics", label = "Physics"),
            #numericInput("gravK", label = "grav constant", value = -2000),
            #numericInput("cgrav", label = "central grav", value = 1),
            #numericInput("speed", label = "speed", value = 25),
            #tagList(
            #        span("Physics"),
            #        switchInput("physics")
            #        ),
            NULL
        ),

        mainPanel(
            visNetworkOutput("network", height="100vh"),
            #tableOutput('selcoltable'),
            #tableOutput('selcoltable2'),
            NULL
        )
    )
)

############################################################
##### APP

shinyApp(ui = ui, server = server)
