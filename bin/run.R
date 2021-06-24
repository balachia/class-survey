#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)

apps <- list(
    `app` = './app',
    `report-app` = './report-app',
    `demo-app` = './demo-app'
)

if(length(args) > 0) {
    app.arg <- args[1]
    if(app.arg %in% names(apps)) {
        dir.app <- apps[[app.arg]]
    } else {
        stop(paste('app must be one of "', paste(names(apps), collapse = '" "'), '"\n', sep = ''))
    }
} else {
    dir.app <- apps[['app']]
}

shiny::runApp(dir.app)
