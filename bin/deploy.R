#!/usr/bin/env Rscript


args <- commandArgs(trailingOnly=TRUE)

apps <- list(
    `app` = list(local = "./app", remote = "ob2-survey"),
    `app-dev` = list(local = "./app", remote = "ob2-survey-dev"),
    `report-app` = list(local = "./report-app", remote = "ob2-reports"),
    `demo-app` = list(local = "./demo-app", remote = "ob2-demo")
)

if(length(args) > 0) {
    app.arg <- args[1]
    if(app.arg %in% names(apps)) {
        local.app <- apps[[app.arg]]$local
        remote.app <- apps[[app.arg]]$remote
    } else {
        stop(paste('app must be one of "', paste(names(apps), collapse = '" "'), '"\n', sep = ''))
    }
} else {
    local.app <- apps[["app"]]$local
    remote.app <- apps[["app"]]$remote
}

rsconnect::deployApp(local.app,
                     appName = remote.app)
