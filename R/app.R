# Project:   startApp
# Objective: Function to run app
# Author:    Edoardo Costantini
# Created:   2022-12-08
# Modified:  2022-12-08
# Notes:

#' startApp
#'
#' Starts a Shiny app to interact with the results of the \href{https://github.com/EdoardoCostantini/mi-spcr}{mi-spcr} project.
#'
#' @details
#' The interface of the Shiny app allows you to change the values of the following simulation study experimental factors:
#' 
#' - Number of latent variables used to generate the data (2, 10, 50)
#' - Proportion of missing values imposed on every variable (0.1, 0.25, 0.5)
#' - Missing data mechanism imposed (MCAR, MAR)
#' - Imputation methods (see names in the interface)
#' 
#'      - pcr: mice with principal component regression as univariate imputation method
#'      - spcr: mice with supervised principal component regression as univariate imputation method
#'      - plsr: mice with partial least squares regression as univariate imputation method
#'      - pcovr: mice with principal covariates regression as univariate imputation method
#'      - qp: mice with normal linear model with bootstrap as univariate imputation method and quickpred() used to select the predictors
#'      - am: mice with normal linear model with bootstrap as univariate imputation method and the analysis model variables used as predictors
#'      - all: mice with normal linear model with bootstrap as univariate imputation method and all available items used as predictors
#'      - cc: complete case analysis
#'      - fo: fully observed data (results if there had been no missing values)
#' 
#' - Number of principal components (depending on the number of latent variables used)
#' - Outcome measure:
#' 
#'      - RB: raw bias
#'      - PRB: percent relative bias
#'      - CIC: confidence interval coverage
#'      - CIW: average confidence interval
#'      - mcsd: Standard deviation of the estimate across the monte carlo simulations 
#' 
#' - Statistic;
#' 
#'      - cor: correlation between two items with missing values
#'      - cov: covariance between two items with missing values
#'      - cor: mean of an item with missing values
#'      - cor: variance of an item with missing values
#' 
#' - Variables considered
#' - Zoom on the y-axis
#' 
#' @export
#' @import shiny
#' @import shinybrowser
#' @import dplyr
#' @import ggplot2
#' @import shinyWidgets
#' @import pkgload
#' @examples startApp()
#' @return Shiny app UI.
#'
startApp <- function(...) {
    # Set up -------------------------------------------------------------------

    # Graph structure
    plot_x_axis <- "npcs"
    plot_y_axis <- "CIC"
    moderator <- "method"
    grid_x_axis <- "mech"
    grid_y_axis <- "pm"
    
    # UI -----------------------------------------------------------------------

    ui <- fluidPage(
        fluidRow(

            # Data generation ----------------------------------------------------------
            column(
                3,
                hr(),
                h4("Data generation"),
                radioButtons("nla",
                    "Number of latent variables",
                    choices = sort(unique(ggshape$nla)),
                    inline = TRUE
                ),
                checkboxGroupInput("pm",
                    "Proportion of missing values",
                    choices = sort(unique(ggshape$pm)),
                    selected = sort(unique(ggshape$pm)),
                    inline = TRUE
                ),
                checkboxGroupInput("mech",
                    "Missing data mechanism",
                    inline = TRUE,
                    choices = levels(ggshape$mech),
                    selected = levels(ggshape$mech)
                ),
            ),

            # Missing data treatments --------------------------------------------------

            column(
                3,
                hr(),
                h4("Missing data treatments"),
                checkboxGroupInput("method",
                    "Imputation methods to compare:",
                    choices = levels(ggshape$method),
                    selected = levels(ggshape$method)[1:4],
                    inline = TRUE
                ),
                shinyWidgets::sliderTextInput(
                    inputId = "npcs",
                    label = "Number of principal components",
                    hide_min_max = TRUE,
                    choices = sort(unique(ggshape$npcs)),
                    selected = range(ggshape$npcs),
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
                    choices = c("RB", "PRB", "CIC", "CIW", "mcsd")
                ),
                radioButtons("stat",
                    "Statistic",
                    inline = TRUE,
                    choices = unique(ggshape$stat)
                ),
                radioButtons("vars",
                    "Variables",
                    inline = TRUE,
                    choices = unique(ggshape$vars)
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

    # Server -------------------------------------------------------------------

    server <- function(input, output, session) {
        # Dynamically update inputs

        # Page width
        observe({
            if (shinybrowser::get_width() < 768) {
                updateCheckboxGroupInput(session,
                    inputId = "mech",
                    selected = levels(ggshape$mech)[2]
                )
            }
        })

        # Statistics and Variables requested
        observe({
            choices_vars <- unique((ggshape %>% filter(stat == input$stat))$vars)

            updateRadioButtons(session,
                inputId = "vars",
                inline = TRUE,
                choices = choices_vars,
                selected = choices_vars[1]
            )
        })

        # Zoom on the y-axis
        observe({
            # Define subset of data in use
            data_subset <- ggshape %>%
                filter(
                    nla == input$nla,
                    mech %in% input$mech,
                    pm %in% input$pm,
                    # vars == input$vars,
                    stat == input$stat,
                    method %in% input$method,
                    npcs <= input$npcs[2],
                    npcs >= input$npcs[1]
                )

            # Define low bound
            b_low <- floor(min(data_subset[, input$plot_y_axis]))
            c_low <- round(min(data_subset[, input$plot_y_axis]), 3)

            # Define high bound
            b_high <- ceiling(max(data_subset[, input$plot_y_axis]))
            c_high <- round(max(data_subset[, input$plot_y_axis]), 3)

            # Define interval
            interval <- ifelse(input$plot_y_axis == "RB" | input$plot_y_axis == "CIC", .1, 1)

            # Choices
            choices <- sort(unique(c(c_low, seq(b_low, b_high, by = interval), c_high)))

            shinyWidgets::updateSliderTextInput(session,
                inputId = "yrange",
                choices = choices,
                selected = c(c_low, c_high)
            )
        })

        # Number of components displayed by slider based on nla condition
        observe({
            npcs_to_plot <- unique((ggshape %>% filter(nla == input$nla))$npcs)
            npcs_to_plot <- sort(npcs_to_plot)
            shinyWidgets::updateSliderTextInput(session,
                inputId = "npcs",
                choices = npcs_to_plot,
                selected = range(npcs_to_plot)
            )
        })

        output$plot <- renderPlot(
            res = 96,
            height = 750,
            {
                ggshape %>%
                    filter(
                        nla == input$nla,
                        mech %in% input$mech,
                        pm %in% input$pm,
                        vars == input$vars,
                        stat == input$stat,
                        method %in% input$method,
                        npcs <= input$npcs[2],
                        npcs >= input$npcs[1]
                    ) %>%
                    ggplot(aes_string(
                        x = plot_x_axis,
                        y = input$plot_y_axis,
                        group = moderator
                    )) +
                    geom_line(col = "darkgray") +
                    geom_point(aes_string(shape = moderator), size = 2.5) +
                    scale_x_continuous(breaks = sort(unique(ggshape$npcs)), sort(unique(ggshape$npcs))) +
                    # Zoomable y-axis
                    coord_cartesian(ylim = c(input$yrange[1], input$yrange[2])) +
                    # Facet grid
                    facet_grid(
                        reformulate(
                            grid_x_axis,
                            grid_y_axis
                        ),
                        labeller = labeller(
                            .rows = label_both,
                            .cols = label_value
                        ),
                        switch = "y"
                    ) +
                    theme(
                        # Text
                        text = element_text(size = 12),
                        strip.text.y.right = element_text(angle = 0),
                        plot.title = element_text(hjust = 0.5),
                        axis.title = element_text(size = 10),
                        axis.title.x = element_blank(),
                        # Legend
                        legend.title = element_blank(),
                        legend.position = "bottom",
                        # Backgorund
                        panel.background = element_rect(fill = NA, color = "gray")
                    )
            }
        )
    }

    # Run app ------------------------------------------------------------------

    shinyApp(ui, server, ...)
}