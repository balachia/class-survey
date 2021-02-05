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

df.qualtrics <- qualtRics::read_survey(unzip('../test/ZKC Qualtrics.zip'))

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
    df.nodes <- df.nodes %>%
        mutate(maybeLabel = ifelse(consent == kConsent, FullName, '???'),
               bConsent = consent == kConsent) %>%
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
    V(g)$centrality.outdegree <- degree(g0, mode = 'in')
    V(g)$centrality.indegree <- degree(g0, mode = 'out')
    V(g)$centrality.close <- closeness(g)
    V(g)$centrality.between <- betweenness(g)
    V(g)$centrality.eigen <- eigen_centrality(g)$vector
    g
}

# set node color by centrality metric
set_node_color <- function(metric, gd) {
    cent <- switch(metric,
                   eigen=rescale(gd$centrality.eigen),
                   between=rescale(gd$centrality.between),
                   close=rescale(gd$centrality.close),
                   indegree=rescale(gd$centrality.indegree),
                   outdegree=rescale(gd$centrality.outdegree),
                   0)
    # restrict observed range to [0.5, 1]
    cent <- 0.5 + 0.5 * cent
    # set colors
    gd$color.background <- color.pal()(cent)
    #gd$color.border <- color.pal()(cent)
    gd$color.highlight.background <- color.pal()(cent)
    gd$color.highlight.border <- color.pal()(cent)
    gd
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
        # export networks
        c(res.full, res.advice, res.support)
    })

    ntwk <- reactive({
        qntwk <- qualtricsNetworks()
        # reset radio buttons
        #updateRadioGroupButtons(session, 'colorMetric', selected = kColorButtonDefault)
        #updateRadioGroupButtons(session, 'colorMetric', selected = kColorButtonDefault)
        # extract and augment correct network
        g <- qntwk[[paste0(input$networkTie, input$networkDirection)]] %>%
            add_layout_(with_fr()) %>%
            toVisNetworkData(idToLabel = FALSE)
        g
    })

    output$network <- renderVisNetwork({
        g <- ntwk()
        g$nodes <- set_node_color(kColorButtonDefault, g$nodes)
        visNetwork(nodes = g$nodes, edges = g$edges) %>%
            visNodes(size = kNodeSizeBase,
                     borderWidth = 2,
                     color = list(border = "white", highlight = list(border = "white"))) %>%
            visEdges(color = "#aaa") %>%
            #visOptions(nodesIdSelection = list(enabled = TRUE)) %>%
            visEvents(selectNode = 'function(properties) { Shiny.setInputValue("selectedNodes", properties.nodes); console.log(properties) }') %>%
            visEvents(deselectNode = 'function(properties) { Shiny.setInputValue("selectedNodes", properties.nodes); console.log(properties) }') %>%
            visEvents(type = 'once', afterDrawing = 'function(properties) { Shiny.setInputValue("actionRedraw", true, {"priority":"event"}); console.log("redraw"); console.log(properties) }') %>%
            visExport()
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

    # handle node sizes

    # update colors by centrality
    observe({
        input$actionRedraw
        g <- ntwk()
        g$nodes <- set_node_color(input$colorMetric, g$nodes)
        # update nodes
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
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

    # highlight revealable people with size
    observe({
        input$actionRedraw
        g <- ntwk()
        if('highlight' %in% input$consenters) {
            g$nodes$size <- ifelse(g$nodes$consent, kNodeSizeBase, kNodeSizeReduced)
        } else {
            g$nodes$size <- kNodeSizeBase
        }
        visNetworkProxy("network") %>%
            visUpdateNodes(g$nodes)
    })

    output$downloadData <- downloadHandler(
        filename = "section-network.graphml",
        content = function(file) {
            igraph::write_graph(qualtricsNetworks()$full, file, format = "graphml")
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
                              choiceNames = c(kColorButtonDefault, 'Out-Degree', 'In-Degree', 'Closeness', 'Betweenness', 'Eigenvector'),
                              choiceValues = c(kColorButtonDefault, 'outdegree', 'indegree', 'close', 'between', 'eigen'),
                              selected = kColorButtonDefault),
            checkboxGroupButtons('consenters',
                                 'Reveal names:',
                                 justified = TRUE,
                                 #status = 'info',
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                 choiceNames = c('Highlight consenters', 'Reveal names'),
                                 choiceValues = c('highlight', 'reveal')),
            NULL
        ),

        mainPanel(
            visNetworkOutput("network", height="600px"),
            tableOutput('selcoltable'),
            tableOutput('selcoltable2'),
            NULL
        )
    )
)

############################################################
##### APP

shinyApp(ui = ui, server = server)
