#' User interface call
#'
#' Calls the definition of the user interface and returns it as an output
#'
#' @return UI object that can be passed directly to shiny::shinyApp()
#' @author Edoardo Costantini, 2023
#' @export
ui_call <- function() {
    # Define ui object
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
                                        title = "1. Simulation outcomes",
                                        selectInput(
                                            inputId = "plot_sim_y_axis",
                                            label = "Outcome measure",
                                            choices = c("RB", "PRB", "CIC", "CIW", "mcsd")[2]
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
                                            selected = c(0, 60),
                                            grid = FALSE
                                        )
                                    ),
                                    shiny::tabPanel(
                                        title = "2. Data generation",
                                        radioButtons(
                                            inputId = "plot_sim_nla",
                                            label = "Number of latent variables",
                                            choices = sort(unique(dataResults$nla)),
                                            selected = sort(unique(dataResults$nla))[2],
                                            inline = TRUE
                                        ),
                                        checkboxGroupInput(
                                            inputId = "plot_sim_pm",
                                            label = "Proportion of missing values",
                                            choices = sort(unique(dataResults$pm)),
                                            selected = sort(unique(dataResults$pm))[c(2, 3)],
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
                                        title = "3. Missing data treatments",
                                        checkboxGroupInput("plot_sim_method",
                                            "Imputation methods to compare:",
                                            choices = levels(dataResults$method),
                                            selected = levels(dataResults$method)[c(1:4, 8)],
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
                                shiny::HTML(
                                    "<br>
                                    This tab allows you to study the <b>trace plots</b> for the MI algorithms used in the simulation study.
                                    The convergence check was performed for the most challenging <b>data condition</b>:
                                    <ul>
                                         <li>50 latent variables</li>
                                         <li>0.5 proportion of missing cases</li>
                                         <li>MAR missing data mechanism</li>
                                    </ul>
                                    For every imputation method, you can check the trace plot with <b>different numbers of PCs</b>.
                                    You can also easily change the <b>range of iterations</b> considered.
                                    <br>
                                    <br>"
                                ),
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
                            ),
                            shiny::column(
                                width = 8,
                                shiny::fluidRow(
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

    # Return ui object
    return(ui)
}