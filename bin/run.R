#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)

apps <- list(
    `app` = './app',
    `report-app` = './report-app',
    `demo-app` = './demo-app'
)

app.arg <- if(length(args) > 0) args[1] else "app"
if(app.arg %in% names(apps)) {
    dir.app <- apps[[app.arg]]
} else {
    stop(paste('app must be one of "', paste(names(apps), collapse = '" "'), '"\n', sep = ''))
}

# stabilize port
port.offset <- which(names(apps) == app.arg) - 1
shiny::runApp(dir.app, port = 35416 + port.offset)
