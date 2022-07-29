# Project:   shiny-mi-spcr-plot
# Objective: script to test and upload app
# Author:    Edoardo Costantini
# Created:   2022-07-29
# Modified:  2022-07-29

# Packages ---------------------------------------------------------------------

  library(shiny)
  library(ggplot2)
  library(shinyWidgets)
  library(dplyr)

# Run app ----------------------------------------------------------------------

  runApp()

# Send to server ---------------------------------------------------------------

  rsconnect::deployApp()