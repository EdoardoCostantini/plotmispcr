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

gg_shape <- readRDS("./20220728-185933-check-pcr-spcr-work-pc-main-res.rds")

plot_x_axis <- "npcs"
plot_y_axis <- "coverage"
moderator <- "method"
grid_x_axis <- "mech"
grid_y_axis <- "pm"

ui <- fluidPage(
  fluidRow(
    column(4,
           hr(),
           h4("Data generation"),
           radioButtons("nla",
                        "Number of latent variables",
                        choices = sort(unique(gg_shape$nla)),
                        inline = TRUE),
           checkboxGroupInput("pm",
                              "Proportion of missing values",
                              choices = sort(unique(gg_shape$pm)),
                              selected = sort(unique(gg_shape$pm)),
                              inline = TRUE),
           checkboxGroupInput("mech",
                              "Missing data mechanism",
                              inline = TRUE,
                              choices = levels(gg_shape$mech),
                              selected = levels(gg_shape$mech)),
    ),
    column(4,
           hr(),
           h4("Outcome measures"),
           radioButtons("stat",
                        "Statistic",
                        inline = TRUE,
                        choices = unique(gg_shape$stat)),
           radioButtons("vars",
                        "Variables",
                        inline = TRUE,
                        choices = unique(gg_shape$vars)),
           selectInput("plot_y_axis",
                       "Outcome measure",
                       choices = c("RB", "PRB", "coverage", "CIW_avg", "mcsd")),
    ),
    column(4,
           hr(),
           h4("Missing data treatments"),
           checkboxGroupInput("method",
                              "Imputation methods to compare:",
                              choices = levels(gg_shape$method),
                              selected = levels(gg_shape$method)[1:4],
                              inline = TRUE),
           shinyWidgets::sliderTextInput(inputId = "npcs",
                                         label = "Number of principal components",
                                         hide_min_max = TRUE,
                                         choices = sort(unique(gg_shape$npcs)),
                                         selected = range(gg_shape$npcs),
                                         grid = TRUE),
    ),
  ),
    hr(),

    plotOutput('plot'),

    # Silent extraction of size
    shinybrowser::detect(),
)