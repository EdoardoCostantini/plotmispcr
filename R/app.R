# Project:   plot.mi.spcr
# Objective: Function to run app to interact with simulation results
# Author:    Edoardo Costantini
# Created:   2022-12-08
# Modified:  2022-12-16
# Notes:

#' plotResults
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
#' - Statistic:
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
#' @return Shiny app UI.
#'
plotResults <- function() {
    # Set up -------------------------------------------------------------------

    # Graph structure
    plot_x_axis <- "npcs"
    plot_sim_y_axis <- "CIC"
    moderator <- "method"
    grid_x_axis <- "mech"
    grid_y_axis <- "pm"

    # UI -----------------------------------------------------------------------

    # App title

    ui <- fluidPage(
        fluidRow(
            shiny::titlePanel(
                shiny::h1("MI-SPCR simulation study results", align = "center")
            ),
            shiny::column(
                width = 10,
                offset = 1,
                # Create tabs for different plotting aspects
                shiny::tabsetPanel(
                    type = "tabs",
                    selected = "About this Shiny app",
                    shiny::tabPanel(
                        title = "About this Shiny app",
                        "Coming soon"
                    ),
                    shiny::tabPanel(
                        title = "Simulation study",
                        shiny::fluidRow(
                            shiny::column(
                                width = 4,
                                shiny::titlePanel(
                                    shiny::h3("Simulation  study", align = "center")
                                ),
                                shiny::tabsetPanel(
                                    type = "tabs",
                                    shiny::tabPanel(
                                        title = "Introduction",
                                        shiny::htmlOutput("introduction")
                                    ),
                                    shiny::tabPanel(
                                        title = "Outcome measures",
                                        selectInput(
                                            inputId = "plot_sim_y_axis",
                                            label = "Outcome measure",
                                            choices = c("RB", "PRB", "CIC", "CIW", "mcsd")
                                        ),
                                        radioButtons(
                                            inputId = "plot_sim_stat",
                                            label = "Statistic",
                                            inline = TRUE,
                                            choices = unique(dataResults$stat)
                                        ),
                                        radioButtons(
                                            inputId = "plot_sim_vars",
                                            label = "Variables",
                                            inline = TRUE,
                                            choices = unique(dataResults$vars)
                                        ),
                                        shinyWidgets::sliderTextInput(
                                            inputId = "plot_sim_y_range",
                                            label = "Y-axis range",
                                            hide_min_max = FALSE,
                                            choices = 0:100,
                                            selected = c(0, 10),
                                            grid = FALSE
                                        )
                                    ),
                                    shiny::tabPanel(
                                        title = "Data generation",
                                        radioButtons(
                                            inputId = "plot_sim_nla",
                                            label = "Number of latent variables",
                                            choices = sort(unique(dataResults$nla)),
                                            inline = TRUE
                                        ),
                                        checkboxGroupInput(
                                            inputId = "plot_sim_pm",
                                            label = "Proportion of missing values",
                                            choices = sort(unique(dataResults$pm)),
                                            selected = sort(unique(dataResults$pm)),
                                            inline = TRUE
                                        ),
                                        checkboxGroupInput("plot_sim_mech",
                                            "Missing data mechanism",
                                            inline = TRUE,
                                            choices = levels(dataResults$mech),
                                            selected = levels(dataResults$mech)
                                        )
                                    ),
                                    shiny::tabPanel(
                                        title = "Missing data treatments",
                                        checkboxGroupInput("plot_sim_method",
                                            "Imputation methods to compare:",
                                            choices = levels(dataResults$method),
                                            selected = levels(dataResults$method)[1:4],
                                            inline = TRUE
                                        ),
                                        shinyWidgets::sliderTextInput(
                                            inputId = "plot_sim_npcs",
                                            label = "Number of principal components",
                                            hide_min_max = TRUE,
                                            choices = sort(unique(dataResults$npcs)),
                                            selected = range(dataResults$npcs),
                                            grid = TRUE
                                        )
                                    )
                                )
                            ),
                            shiny::column(
                                width = 8,
                                shiny::fluidRow(
                                    shiny::titlePanel(
                                        shiny::h3("Plots", align = "center")
                                    ),
                                    shiny::plotOutput("plot"),

                                    # Silent extraction of size
                                    shinybrowser::detect(),
                                ),
                                style = "border-left: 1px solid; border-left-color: #DDDDDD"
                            )
                        )
                    ),
                    shiny::tabPanel(
                        title = "Convergence checks",
                        shiny::fluidRow(
                            shiny::column(
                                width = 4,
                                shiny::titlePanel(
                                    shiny::h3("Simulation  study", align = "center")
                                ),
                                shiny::tabsetPanel(
                                    type = "tabs",
                                    shiny::tabPanel(
                                        title = "Introduction",
                                        ""
                                    ),
                                    shiny::tabPanel(
                                        title = "Inputs",
                                        selectInput("plot_case_method",
                                            "Imputation method:",
                                            choices = levels(dataResults$method)[1:7],
                                            selected = levels(dataResults$method)[1]
                                        ),
                                        selectInput(
                                            inputId = "plot_case_npcs",
                                            label = "Number of PCs used",
                                            choices = sort(unique(dataResults$npcs))[-1],
                                            selected = sort(unique(dataResults$npcs))[2]
                                        ),
                                        shinyWidgets::sliderTextInput(
                                            inputId = "plot_case_iters",
                                            label = "Iteration range",
                                            hide_min_max = TRUE,
                                            choices = 0:100,
                                            selected = c(0, 25),
                                            grid = FALSE
                                        )
                                    )
                                )
                            ),
                            shiny::column(
                                width = 8,
                                shiny::fluidRow(
                                    shiny::titlePanel(
                                        shiny::h3("Plots", align = "center")
                                    ),
                                    shiny::plotOutput("plot_mids"),

                                    # Silent extraction of size
                                    shinybrowser::detect(),
                                ),
                                style = "border-left: 1px solid; border-left-color: #DDDDDD"
                            )
                        )
                    )
                )
            )
        )
    )

    # Server -------------------------------------------------------------------

    server <- function(input, output, session) {
        # Dynamically update inputs

        # Page width
        observe({
            if (shinybrowser::get_width() < 768) {
                updateCheckboxGroupInput(session,
                    inputId = "mech",
                    selected = levels(dataResults$mech)[2]
                )
            }
        })

        # Statistics and Variables requested
        observe({
            choices_vars <- unique(
                (dataResults %>%
                    filter(stat == input$plot_sim_stat)
                )$vars
            )

            updateRadioButtons(session,
                inputId = "plot_sim_vars",
                inline = TRUE,
                choices = choices_vars,
                selected = choices_vars[1]
            )
        })

        # Zoom on the y-axis
        observe({
            # Define subset of data in use
            data_subset <- dataResults %>%
                filter(
                    nla == input$plot_sim_nla,
                    mech %in% input$plot_sim_mech,
                    pm %in% input$plot_sim_pm,
                    stat == input$plot_sim_stat,
                    method %in% input$plot_sim_method,
                    npcs <= input$plot_sim_npcs[2],
                    npcs >= input$plot_sim_npcs[1]
                )

            # Define low bound
            b_low <- floor(min(data_subset[, input$plot_sim_y_axis]))
            c_low <- round(min(data_subset[, input$plot_sim_y_axis]), 3)

            # Define high bound
            b_high <- ceiling(max(data_subset[, input$plot_sim_y_axis]))
            c_high <- round(max(data_subset[, input$plot_sim_y_axis]), 3)

            # Define interval
            interval <- ifelse(input$plot_sim_y_axis == "RB" | input$plot_sim_y_axis == "CIC", .1, 1)

            # Choices
            choices <- sort(unique(c(c_low, seq(b_low, b_high, by = interval), c_high)))

            shinyWidgets::updateSliderTextInput(session,
                inputId = "plot_sim_y_range",
                choices = choices,
                selected = c(c_low, c_high)
            )
        })

        # Number of components displayed by slider based on nla condition
        observe({
            npcs_to_plot <- unique(
                (dataResults %>%
                    filter(nla == input$plot_sim_nla)
                )$npcs
            )
            npcs_to_plot <- sort(npcs_to_plot)
            shinyWidgets::updateSliderTextInput(session,
                inputId = "plot_sim_npcs",
                choices = npcs_to_plot,
                selected = range(npcs_to_plot)
            )
        })

        output$plot <- renderPlot(
            res = 96,
            height = 750,
            {
                dataResults %>%
                    filter(
                        nla == input$plot_sim_nla,
                        mech %in% input$plot_sim_mech,
                        pm %in% input$plot_sim_pm,
                        vars == input$plot_sim_vars,
                        stat == input$plot_sim_stat,
                        method %in% input$plot_sim_method,
                        npcs <= input$plot_sim_npcs[2],
                        npcs >= input$plot_sim_npcs[1]
                    ) %>%
                    ggplot(aes_string(
                        x = plot_x_axis,
                        y = input$plot_sim_y_axis,
                        group = moderator
                    )) +
                    geom_line(col = "darkgray") +
                    geom_point(aes_string(shape = moderator), size = 2.5) +
                    scale_x_continuous(
                        breaks = sort(unique(dataResults$npcs)), 
                        sort(unique(dataResults$npcs))
                        ) +

                    # Zoomable y-axis
                    coord_cartesian(ylim = c(
                        input$plot_sim_y_range[1], 
                        input$plot_sim_y_range[2])
                        ) +

                    # Facet grid
                    facet_grid(
                        stats::reformulate(
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

output$plot_mids <- renderPlot(
            res = 96,
            height = 750,
            {
                # Define condition to plot based on inputs
                cnd_search <- paste0(
                    "npcs-", input$plot_case_npcs,
                    "-method-", input$plot_case_method
                )
                cnd_id <- grep(cnd_search, names(dataMids$mids))

                # Work with simple object name
                x <- dataMids$mids[[cnd_id]]

                # Default arguments that you could change in MICE
                type <- "l"
                col <- 1:10
                lty <- 1
                theme <- mice::mice.theme()
                layout <- c(2, 3)

                # Extract objects I need
                mn <- x$chainMean
                sm <- sqrt(x$chainVar)
                m <- x$m
                it <- x$iteration

                # select subset of non-missing entries
                obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
                varlist <- names(obs)[obs]

                # Prepare objects for plotting
                mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
                sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 1)), nrow = m * it)
                adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
                data <- cbind(adm, rbind(mn, sm))
                colnames(data) <- c(".it", ".m", ".ms", varlist)

                # Create formula
                formula <- as.formula(paste0(
                    paste0(varlist, collapse = "+"),
                    "~.it|.ms"
                ))

                # Dummy to trick R CMD check
                .m <- NULL
                rm(.m)

                # Load function to obtain the correct plot arrangement
                strip.combined <- function(which.given, which.panel, factor.levels, ...) {
                    if (which.given == 1) {
                        lattice::panel.rect(0, 0, 1, 1,
                            col = theme$strip.background$col, border = 1
                        )
                        lattice::panel.text(
                            x = 0, y = 0.5, pos = 4,
                            lab = factor.levels[which.panel[which.given]]
                        )
                    }
                    if (which.given == 2) {
                        lattice::panel.text(
                            x = 1, y = 0.5, pos = 2,
                            lab = factor.levels[which.panel[which.given]]
                        )
                    }
                }

                # Make plot
                lattice::xyplot(
                    x = formula, data = data, groups = .m,
                    type = type, lty = lty, col = col, layout = layout,
                    scales = list(
                        y = list(relation = "free"),
                        x = list(alternating = FALSE)
                    ),
                    as.table = TRUE,
                    xlim = c(input$plot_case_iters[1] - 1, input$plot_case_iters[2] + 1),
                    xlab = "Iteration",
                    ylab = "",
                    strip = strip.combined,
                    par.strip.text = list(lines = 0.5),
                )
            }

        )
    }

    # Run app ------------------------------------------------------------------

    shinyApp(ui, server)
}
