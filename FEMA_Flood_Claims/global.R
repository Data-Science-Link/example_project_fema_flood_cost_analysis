# library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

#GGplot specifications
title_text_sz = 18
axis_text_sz = 10
axis_title_sz = 12

load(file = "processed_data.Rdata")