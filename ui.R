library(shiny)
library(tidyverse)
library(reactable)
library(keys)
library(jsonlite)
library(lubridate)
library(plotly)


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
           column(7, div("Dataset"), reactableOutput('dataset')),
           column(1, div("current_users"), reactableOutput('current_users')),
           column(2,
                  fluidRow(reactableOutput('nodes')),
                  fluidRow(reactableOutput('links')),
                  fluidRow(reactableOutput('snk_nodes')),
                  fluidRow(reactableOutput('snk_links')))
    ),
    column(3, plotlyOutput("snk_plot"))
  )
)