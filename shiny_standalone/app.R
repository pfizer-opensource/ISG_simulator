# Standalone Shiny App for Power Analysis
# This version is configured for deployment to shinyapps.io
# 
# Setup: This app is self-contained and uses relative paths that work 
# from within the shiny_standalone directory

# Resolve the app directory from this script so the app can be sourced from any
# working directory.
get_current_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(active_path)) {
      return(normalizePath(active_path, winslash = "/", mustWork = TRUE))
    }
  }

  return(NA_character_)
}

app_script_path <- get_current_script_path()
.shiny_app_root <- if (!is.na(app_script_path)) dirname(app_script_path) else getwd()

# Source helper functions for path setup
source(file.path(.shiny_app_root, "app_helper.R"))

# Load required libraries
library(shiny)
library(shinydashboard)
library(mathjaxr)
library(bslib)
library(gt)
library(dplyr)
library(pwr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(rstatix)                                         # for anova_test()
library(emmeans)
library(ggpubr)
library(lme4)
library(parallel)
library(doParallel)

# Source power analysis functions
app_source("functions/pwr_ana_hc_asc.R")
app_source("functions/pwr_ana_hc_par.R")
app_source("functions/pwr_ana_hc_par_rm.R")
app_source("functions/pwr_ana_hc_seq.R")
app_source("functions/pwr_ana_hc_xov.R")
app_source("script/functions/f_list_to_tb.R")
app_source("script/import_power_tb_sim.R")

# Set up resource path for images
addResourcePath("pics", file.path(app_root, "ui/pics"))

# Build UI
ui <- dashboardPage(
  dashboardHeader(title = "Safety Pharm Design Simulator"),
  
  dashboardSidebar(width = 350,
                   {
                     sidebarMenu(
                       menuItem(   "App Introduction"         , tabName = "p0"   ),
                       menuItem(   "1. Parallel Design"       , tabName = "p1"   ),
                       menuItem(   "2. Parallel-RM Design"    , tabName = "p2"   ),
                       menuItem(   "3. Crossover Design"      , tabName = "p3"   ),
                       menuItem(   "4. Sequential Design"     , tabName = "p4"   ),
                       menuItem(   "5. Ascending Design"      , tabName = "p5"   ),
                       menuItem(   "6. Modified Ascending Design" , tabName = "p6"   ),
                       menuItem(   "7. Modified Ascending Design 2" , tabName = "p7"   )
                     )
                   }
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("\
        .main-header .logo {\
          width: 360px;\
          white-space: nowrap;\
          overflow: visible;\
          text-overflow: clip;\
          font-size: 18px;\
        }\
        .main-header .navbar {\
          margin-left: 360px;\
        }\
        @media (max-width: 767px) {\
          .main-header .logo {\
            width: 100%;\
            font-size: 16px;\
          }\
          .main-header .navbar {\
            margin-left: 0;\
          }\
        }\
      "))
    ),
    tabItems(
      
      ##### p0 [App Intro and Overall Settings] #####
      app_source("ui/ui_p0.R", local = TRUE)$value,
      
      ##### p1  PAR  #####
      app_source("ui/ui_p1.R", local = TRUE)$value, 
      
      ##### p2  PAR-RM  #####
      app_source("ui/ui_p2.R", local = TRUE)$value,
      
      ##### p3  XOV #####
      app_source("ui/ui_p3.R", local = TRUE)$value,
      
      ##### p4  SEQ #####
      app_source("ui/ui_p4.R", local = TRUE)$value,
      
      ##### p5  ASC #####
        app_source("ui/ui_p5.R", local = TRUE)$value,

        ##### p6  MOD-ASC #####
        app_source("ui/ui_p6.R", local = TRUE)$value,

        ##### p7  MOD-ASC1 #####
        app_source("ui/ui_p7.R", local = TRUE)$value
      
     ) 
 )
)
 
# Build Server
server <- function(input, output, session) {
  setwd(app_root)
   
      ##### p0 #####
  app_source("server/server_p0.R", local = TRUE)$value
        
      ##### p1 PAR #####
  app_source("server/server_p1.R", local = TRUE)$value
  
      ##### p2 PAR-RM #####
  app_source("server/server_p2.R", local = TRUE)$value
  
      ##### p3 XOV #####
  app_source("server/server_p3.R", local = TRUE)$value
  
      ##### p4 SEQ #####
  app_source("server/server_p4.R", local = TRUE)$value
  
      ##### p5 ASC #####
  app_source("server/server_p5.R", local = TRUE)$value

        ##### p6 MOD-ASC #####
      app_source("server/server_p6.R", local = TRUE)$value
  
        ##### p7 MOD-ASC1 #####
      app_source("server/server_p7.R", local = TRUE)$value
  
}

# Run the application
shinyApp(ui, server)
