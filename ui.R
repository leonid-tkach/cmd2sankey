library(shiny)
library(tidyverse)
library(reactable)
library(keys)
library(jsonlite)
library(lubridate)


fluidPage(
  useKeys(),
  keysInput('keys', c('enter'), global = TRUE),
  fluidRow(id = 'firstRow',
    column(9,
           column(2,
                  textInput('tmnl', 'Terminal'),
                  actionButton('run', 'Run'),
                  div(style = "max-height: 15vh; overflow-y: auto; font-size: 10px;",
                      htmlOutput('log')),
                  fluidRow(downloadButton('dl_btn', 'Download'),
                           fileInput('ul_btn', NULL, accept = '.json', placeholder = 'Upload your dataset'))
                  ),
           column(8, div("Dataset"), reactableOutput('dataset')),
           column(2, div("current_users"), reactableOutput('current_users'))
    )
  )
)