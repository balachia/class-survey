#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)

apps <- list(
    `app` = list(local = "./app", remote = "ob2-survey"),
    `app-dev` = list(local = "./app", remote = "ob2-survey-dev"),
    `report-app` = list(local = "./report-app", remote = "ob2-reports"),
    `demo-app` = list(local = "./demo-app", remote = "ob2-demo")
)

app.arg <- if(length(args) > 0) args[1] else "app"
if(app.arg %in% names(apps)) {
    local.app <- apps[[app.arg]]$local
    remote.app <- apps[[app.arg]]$remote
} else {
    stop(paste('app must be one of "', paste(names(apps), collapse = '" "'), '"\n', sep = ''))
}

rsconnect::deployApp(local.app,
                     appName = remote.app)
