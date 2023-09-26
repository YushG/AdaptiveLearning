rm(list = ls())

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(rlist)
library(reshape2)
library(stringi)
library(stringr)
library(data.table)
library(ggplot2)

source('ui.R', local = TRUE)

source('server.R')

# Run the app ----
shinyApp(ui = ui, server = server)
