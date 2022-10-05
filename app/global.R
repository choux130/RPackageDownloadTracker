rm(list = ls())
gc()

library(shiny)
library(dataui)
library(reactable)
library(shinyFeedback)
library(shinyWidgets)
library(shinycssloaders)
library(bs4Dash)

library(dplyr)
library(data.table)
library(cranlogs)
source("fun.R")

library(future)
library(future.apply)
plan(multisession)

period_getFakeData_sec = 5
period_generateFakeData_millisec = period_getFakeData_sec * 1000
period_getRealData_sec = 1800
period_getRealData_millisec = period_getRealData_sec * 1000

