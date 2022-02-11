library(shiny)
library(tidyverse)
library(reactable)
library(keys)
library(jsonlite)

fluidPage(
  useKeys(),
  keysInput('keys', c('enter'), global = TRUE),
  fluidRow(id = 'firstRow',
    column(7,
           column(2,
                  textInput('tmnl', 'Terminal'),
                  actionButton('run', 'Run'),
                  div(style = "max-height: 15vh; overflow-y: auto; font-size: 10px;",
                      htmlOutput('log')),
                  fluidRow(downloadButton('dl_btn', 'Download'),
                           fileInput('ul_btn', NULL, accept = '.json', placeholder = 'Upload your dataset'))
                  ),
           column(6, div("Dataset"), reactableOutput('dataset')),
           column(4, div("cmd_log"), reactableOutput('cmd_log'))
    ),
    column(2,
           div("current_users"), reactableOutput('current_users'))
  )
)