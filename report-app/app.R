library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(igraph)
library(visNetwork)
#library(qualtRics)
library(RColorBrewer)
library(networkSurveyBackend)
library(ggplot2)
library(cowplot)
library(gridGraphics)
library(tinytex)

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

############################################################
##### network setup

# set node color by centrality metric
set_node_color <- function(metric, g) {
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

plottable.g <- function(g, vid, scale=1) {
    #nbrs <- igraph::neighbors(g, params$node)
    # access node by name
    vidc <- as.character(vid)
    # set node colors, labels, size
    V(g)$label <- ''
    V(g)$color <- color.pal()(0.3)
    V(g)$size <- scale*9
    # set you
    V(g)[name == vid]$label <- 'You'
    V(g)[name == vid]$color <- color.pal()(0.6)
    V(g)[name == vid]$size <- scale*18
    # set outgoing neighbors
    V(g)[ .outnei(vidc) ]$color <- color.pal()(0.4)
    V(g)[ .outnei(vidc) ]$size <- scale*12
    # set neighbors
    V(g)[ .innei(vidc) ]$color <- color.pal()(0.5)
    V(g)[ .innei(vidc) ]$size <- scale*14
    g
}

g.stats <- function(g) {
    dat <- igraph::as_data_frame(g, 'vertices') %>%
        select(starts_with("centrality") | starts_with("name")) %>%
        select(!c(centrality.outdegree, centrality.indegree)) %>%
        rename(Eigenvector = centrality.eigen,
               Closeness = centrality.close,
               Betweenness = centrality.between,
               Cluster = centrality.cluster) %>%
        pivot_longer(!name, names_to = "centrality") %>%
        group_by(centrality) %>%
        mutate(pctile = rank(value) / n())
    dat
}

netplot <- function(g) {
    function() {
        par(mar=c(0,0,0,0), oma=c(0,0,0,0))
        plot(g,
             margin = 0,
             frame = FALSE,
             vertex.frame.color = 'white',
             vertex.label.family = 'sans',
             edge.color = '#aaaaaa',
             edge.width = 0.5,
             edge.curved = rep(0.00 * c(-1, 1), length.out = length(E(g))))
    }
}

netplot.only <- function(g, vid, ...) {
    gp <- plottable.g(g, vid, ...)
    plot_grid(netplot(gp))
}

pctile.plot <- function(g, vid) {
    mp <- function(x) 0.5 * (max(x) + min(x))
    df <- g.stats(g)
    df.id <- df %>%
        mutate(pctile.label = sprintf('%0.1f%%', 100 * pctile)) %>%
        mutate(lab.h = ifelse(value < mp(value), -0.25, 1.25)) %>%
        filter(name == vid)
    ggp <- df %>% ggplot(aes(value)) +
        theme_void(base_size = 8) +
        theme(plot.margin = margin(0.2, 0, 0.2, 0, 'npc'),
              plot.title = element_text(margin = margin(0, 0, 0.1, 0, 'npc'))) +
        labs(x = NULL, y = NULL, title = 'Network Scores') +
        facet_wrap(~centrality, ncol = 1, scales = 'free') +
        geom_density(color = NA, fill = 'gray') +
        geom_vline(data = df.id, aes(xintercept = value)) +
        geom_text(data = df.id, aes(x = value, y = 0, label = pctile.label,
                                    hjust = lab.h), vjust = -1, size = 2.5) +
        NULL
    ggp
}

combined.plot <- function(g, vid) {
    gp <- plottable.g(g, vid)
    plot_grid(netplot(gp), pctile.plot(gp, vid), nrow = 1, rel_widths = c(3,1))
    #plot_grid(netplot(gp), netplot(gp), nrow = 1, rel_widths = c(3,1))
    #plot_grid(pctile.plot(gp, vid), pctile.plot(gp, vid), nrow = 1, rel_widths = c(3,1))
}

report.factory <- function(file, params, template = "report.Rmd") {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy(template, tempReport, overwrite = TRUE)

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
}

report.handler <- function(inputTemplate, panelFile, networks, max.reports = NULL) {
    function(file) {
        # allow alternate template
        if(is.null(inputTemplate)) {
            template <- 'report.Rmd'
        } else {
            template <- inputTemplate$datapath
        }
        ns <- networks
        nvs <- if(is.null(max.reports)) { length(V(ns$full.directed)) } else { 1 }
        # allow additional info from panel
        if(is.null(panelFile)) {
            node.df <- networks$nodes
        } else {
            panel.df <- read_csv(panelFile$datapath)
            node.df <- networks$nodes %>%
                left_join(panel.df, by = 'NodeID')
        }
        nodes.info <- node.df %>%
            as.list() %>%
            transpose()
        # extract advice and support networks
        ga <- ns$advice.any
        gs <- ns$support.any
        ga <- add_layout_(ga, with_fr())
        gs <- add_layout_(gs, with_fr())
        # create file paths
        rootpath <- tempdir(TRUE)
        dir.create(file.path(rootpath, 'plot'), recursive = TRUE, showWarnings = FALSE)
        # plot paths
        paths.a <- sprintf('plot/advice-%03d.png', 1:nvs)
        paths.s <- sprintf('plot/support-%03d.png', 1:nvs)
        fullpaths.a <- file.path(rootpath, paths.a)
        fullpaths.s <- file.path(rootpath, paths.s)
        # report temporary and final paths
        paths.report <- sprintf('report-%03d', 1:nvs)
        final.paths.report <- character(nvs)
        withProgress(message = 'Generating reports',
                     value = 0,
                     detail = paste(0, '/', nvs), 
            {
                for(i in 1:nvs) {
                    ggp.a <- combined.plot(ga, i)
                    ggsave(fullpaths.a[[i]], ggp.a, width = 5, height = 3, dpi = 300)
                    ggp.s <- combined.plot(gs, i)
                    ggsave(fullpaths.s[[i]], ggp.s, width = 5, height  = 3, dpi = 300)
                    # create parameters for report
                    params <- list(node = nodes.info[[i]],
                                   plotAdvice = fullpaths.a[[i]],
                                   plotSupport = fullpaths.s[[i]])
                    print(networks$nodes)
                    # get final report filepath
                    final.paths.report[i] <- report.factory(paths.report[[i]], params, template = template)
                    incProgress(1/nvs, detail = paste(i, "/", nvs))
                }
            })
        zip::zip(file, files = c(paths.a, paths.s), root = rootpath, mode = 'mirror')
        zip::zip_append(file, files = c(final.paths.report), mode = 'cherry-pick')
    }
}

############################################################
##### SERVER

server <- function(input, output, session) {
    qualtricsNetworks <- reactive({
        # load network
        req(input$inFile)
        inputSurvey <- input$inFile
        df.qualtrics <- networkSurveyBackend::load_qualtrics_file(inputSurvey)
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
        # extract node characteristics
        nodes.fields <- c('NodeID', 'RecipientFirstName', 'RecipientLastName', 'RecipientEmail', 'FullName', kQuestionSudo, kQuestionConsent)
        nodes.data <- data.frame(NodeID = 1:length(V(g))) %>%
            left_join(select(df.qualtrics, one_of(nodes.fields)), by = 'NodeID')
        # add layouts
        # build full edges network
        ge <- build_edge_network(g)
        # export networks
        c(res.full, res.advice, res.support, list(edges = ge, nodes = nodes.data))
    })

    ntwk <- reactive({
        qntwk <- qualtricsNetworks()
        # reset radio buttons
        netType <- input$networkTie
        netDir <- input$networkDirection
        # extract and augment correct network
        g <- qntwk[[paste0(netType, netDir)]] %>%
            toVisNetworkData(idToLabel = FALSE)
        ge <- set_edge_physics(qntwk[["edges"]], netType, netDir, derivedSizeBases()$edge) %>%
            toVisNetworkData(idToLabel = FALSE)
        list(nodes = g$nodes, edges = ge$edges)
    })

    output$network <- renderVisNetwork({
        qntwk <- qualtricsNetworks()
        g <- isolate(ntwk())
        g$nodes <- set_node_color(isolate(input$colorMetric), g$nodes)
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
            visPhysics(enabled = FALSE) %>%
            #visPhysics(solver = "barnesHut",
            #           maxVelocity = 16,
            #           barnesHut = list(gravitationalConstant = -10000, centralGravity = 3, damping = 0.09),
            #           stabilization = list(iterations = 1000, fit = TRUE)) %>%
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
        g$nodes <- set_node_color(input$colorMetric, g$nodes)
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

    # set various sizes after determining initial graph diameter
    observeEvent(input$actionStabilized, {
        visNetworkProxy("network") %>%
            #visFit(animation = list(duration = 2000)) %>%
            visGetPositions()
    })

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

    #output$report <- downloadHandler(
    #    filename = "reports.zip",
    #    #filename = "reports.html",
    #    content = function(file) {
    #        # allow alternate template
    #        templateFile <- isolate(input$reportFile)
    #        if(is.null(templateFile)) {
    #            template <- 'report.Rmd'
    #        } else {
    #            template <- templateFile$datapath
    #        }
    #        ns <- qualtricsNetworks()
    #        nvs <- length(V(ns$full.directed))
    #        # extract advice and support networks
    #        ga <- ns$advice.any
    #        gs <- ns$support.any
    #        ga <- add_layout_(ga, with_fr())
    #        gs <- add_layout_(gs, with_fr())
    #        # create file paths
    #        #paths <- file.path(tempdir(), sprintf('report-%04d.html', 1:nvs))
    #        rootpath <- tempdir(TRUE)
    #        dir.create(file.path(rootpath, 'plot'), recursive = TRUE, showWarnings = FALSE)
    #        #paths.a <- file.path(rootpath, sprintf('advice-%03d.png', 1:nvs))
    #        #paths.s <- file.path(rootpath, sprintf('support-%03d.png', 1:nvs))
    #        paths.a <- sprintf('plot/advice-%03d.png', 1:nvs)
    #        paths.s <- sprintf('plot/support-%03d.png', 1:nvs)
    #        paths.report <- sprintf('report-%03d', 1:nvs)
    #        fullpaths.a <- file.path(rootpath, paths.a)
    #        fullpaths.s <- file.path(rootpath, paths.s)
    #        final.paths.report <- character(nvs)
    #        withProgress(message = 'Generating reports',
    #                     value = 0,
    #                     detail = paste(0, '/', nvs), 
    #            {
    #                for(i in 1:nvs) {
    #                    ggp.a <- combined.plot(ga, i)
    #                    ggsave(fullpaths.a[[i]], ggp.a, width = 5, height = 3, dpi = 300)
    #                    ggp.s <- combined.plot(gs, i)
    #                    ggsave(fullpaths.s[[i]], ggp.s, width = 5, height  = 3, dpi = 300)
    #                    params <- list(node = i,
    #                                   plotAdvice = fullpaths.a[[i]],
    #                                   plotSupport = fullpaths.s[[i]])
    #                    #ggsave(paths.s[[i]], ggp.s)
    #                    final.paths.report[i] <- report.factory(paths.report[[i]], params, template = template)
    #                    #print(ns$full.directed)
    #                    incProgress(1/nvs, detail = paste(i, "/", nvs))
    #                }
    #            })
    #        print(paths.report)
    #        print(final.paths.report)
    #        #mail.merge.df <- data.frame(NodeID = 1:nvs, advice = paths.a, support = paths.s)
    #        #zip::zip(file, files = c(paths.a, paths.s), mode = 'cherry-pick')
    #        zip::zip(file, files = c(paths.a, paths.s), root = rootpath, mode = 'mirror')
    #        zip::zip_append(file, files = c(final.paths.report), mode = 'cherry-pick')
    #    }
    #)

    output$report <- downloadHandler(
        filename = "reports.zip",
        #filename = "reports.html",
        content = report.handler(input$reportFile, input$panelFile, qualtricsNetworks(), max.reports = NULL)
    )

    output$report1 <- downloadHandler(
        filename = "preview-report.zip",
        content = report.handler(input$reportFile, input$panelFile, qualtricsNetworks(), max.reports = 1)
    )
}

############################################################
##### UI

ui <- fluidPage(
    #shinyjs::useShinyjs(),
    sidebarLayout(
        sidebarPanel(
            titlePanel('Section Network Map Report Generator'),
            fileInput("inFile", "Upload Qualtrics results file (zip/csv)", accept = c(".zip", ".csv")),
            fileInput("panelFile", "Upload section panel file (csv)", accept = c(".csv")),
            fileInput("reportFile", "Upload alternate report template (Rmd)", accept = c(".Rmd")),
            #uiOutput("downloadUI"),
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
                              choiceNames = c(kColorButtonDefault, 'Out-Degree', 'In-Degree', 'Closeness', 'Betweenness', 'Eigenvector', 'Cluster'),
                              choiceValues = c(kColorButtonDefault, 'outdegree', 'indegree', 'close', 'between', 'eigen', 'cluster'),
                              selected = kColorButtonDefault),
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
            tagList(
                p('Generate reports'),
                downloadButton("report1", label = "Preview Report"),
                downloadButton("report", label = "Download All")
            ),
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
