# Project:   shiny-mi-spcr-plot
# Objective: user interface
# Author:    Edoardo Costantini
# Created:   2022-07-27
# Modified:  2022-08-01

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(shinybrowser) # web browser information in Shiny apps

gg_shape <- readRDS("./20221202-105949-results.rds")

plot_x_axis <- "npcs"
plot_y_axis <- "coverage"
moderator <- "method"
grid_x_axis <- "mech"
grid_y_axis <- "pm"

ui <- fluidPage(
  fluidRow(

    # Data generation ----------------------------------------------------------
    column(
      3,
      hr(),
      h4("Data generation"),
      radioButtons("nla",
        "Number of latent variables",
        choices = sort(unique(gg_shape$nla)),
        inline = TRUE
      ),
      checkboxGroupInput("pm",
        "Proportion of missing values",
        choices = sort(unique(gg_shape$pm)),
        selected = sort(unique(gg_shape$pm)),
        inline = TRUE
      ),
      checkboxGroupInput("mech",
        "Missing data mechanism",
        inline = TRUE,
        choices = levels(gg_shape$mech),
        selected = levels(gg_shape$mech)
      ),
    ),

    # Missing data treatments --------------------------------------------------

    column(
      3,
      hr(),
      h4("Missing data treatments"),
      checkboxGroupInput("method",
        "Imputation methods to compare:",
        choices = levels(gg_shape$method),
        selected = levels(gg_shape$method)[1:4],
        inline = TRUE
      ),
      shinyWidgets::sliderTextInput(
        inputId = "npcs",
        label = "Number of principal components",
        hide_min_max = TRUE,
        choices = sort(unique(gg_shape$npcs)),
        selected = range(gg_shape$npcs),
        grid = TRUE
      ),
    ),

    # Outcome measures ---------------------------------------------------------

    column(
      3,
      hr(),
      h4("Outcome measures"),
      selectInput("plot_y_axis",
        "Outcome measure",
        choices = c("RB", "PRB", "coverage", "CIW_avg", "mcsd")
      ),
      radioButtons("stat",
        "Statistic",
        inline = TRUE,
        choices = unique(gg_shape$stat)
      ),
      radioButtons("vars",
        "Variables",
        inline = TRUE,
        choices = unique(gg_shape$vars)
      ),
    ),

    # Zoom on y-axis -----------------------------------------------------------

    column(
      3,
      hr(),
      h4("Zoom on y-axis"),
      shinyWidgets::sliderTextInput(
        inputId = "yrange",
        label = "Y-axis range",
        hide_min_max = FALSE,
        choices = 0:100,
        selected = c(0, 10),
        grid = FALSE
      ),
    ),
  ),
  hr(),
  plotOutput("plot"),

  # Silent extraction of size
  shinybrowser::detect(),
)
