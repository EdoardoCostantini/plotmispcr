# Project:   shiny-mi-spcr-plot
# Objective: TODO
# Author:    Edoardo Costantini
# Created:   2022-07-27
# Modified:  2022-07-28

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)

gg_shape <- readRDS("./20220726-143214-small-run-for-plot-development-pc-main-res.rds")

plot_x_axis <- "npcs"
plot_y_axis <- "coverage"
moderator <- "method"
grid_x_axis <- "mech"
grid_y_axis <- "pm"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("nla",
                  "Number of latent variables",
                  choices = sort(unique(gg_shape$nla))),
      checkboxGroupInput("pm",
                         "Proportion of missing values",
                         choices = sort(unique(gg_shape$pm)),
                         selected = sort(unique(gg_shape$pm))),
      checkboxGroupInput("mech",
                         "Missing data mechanism",
                         inline = TRUE,
                         choices = levels(gg_shape$mech),
                         selected = levels(gg_shape$mech)),
      selectInput("stat",
                  "Statistic",
                  choices = unique(gg_shape$stat)),
      selectInput("vars",
                  "Variables",
                  choices = unique(gg_shape$vars)),
      selectInput("plot_y_axis",
                  "Outcome measure",
                  choices = c("RB", "PRB", "coverage", "CIW_avg", "mcsd")),
      checkboxGroupInput("method", "Imputation methods to compare:",
                         choices = levels(gg_shape$method),
                         selected = levels(gg_shape$method)[1:4]),
      shinyWidgets::sliderTextInput(inputId = "npcs",
                                    label = "Number of principal components",
                                    hide_min_max = TRUE,
                                    choices = sort(unique(gg_shape$npcs)),
                                    selected = range(gg_shape$npcs),
                                    grid = TRUE),
    ),
    mainPanel(
      plotOutput("plot", height = "800px")
    )
  )
)