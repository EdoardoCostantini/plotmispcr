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
        shiny::titlePanel(
            shiny::h1("Supervised dimensionality reduction for MICE", align = "center")
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
                    shiny::column(
                        width = 8,
                        offset = 2,
                        shiny::HTML(
                            "<br>
                            This Shiny app accompanies the paper:
                            <br>
                            <br>
                            Costantini, E., Lang, K. M., Sijtsma, K. (2023). Supervised dimensionality reduction for multiple imputation by chained equations. <i>arXiv preprint arXiv:2309.01608.</i> DOI: <a href='https://doi.org/10.48550/arXiv.2309.01608'>10.48550/arXiv.2309.01608</a>
                            <br>
                            <br>
                            With this app, the user can:
                            <ul>
                                <li>Interact with the simulation study results presented in the paper by using <b>Module 1</b>.</li>
                                <li>Interact with trace plots showing the convergence of the methods compared in the simulation study by using <b>Module 2</b>.</li>
                            </ul>
                            For questions and feedback, please <a href = 'mailto:e.costantini@tilburguniversity.edu'>send me an email</a>.
                            "
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 1: Simulation study",
                    shiny::fluidRow(
                        shiny::column(
                            width = 4,
                            shiny::HTML(
                                "<br>
                                    This tab allows you to plot the results of the simulation study reported in the article.
                                    You change the values of the experimental factors to plot the results you are most interested in.
                                    <br>
                                    <br>"
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    width = 6,
                                    radioButtons(
                                        inputId = "plot_sim_nla",
                                        label = "Number of latent variables",
                                        choices = sort(unique(plotmispcr::dataResults$nla)),
                                        selected = sort(unique(plotmispcr::dataResults$nla))[2],
                                        inline = FALSE
                                    ),
                                    checkboxGroupInput(
                                        inputId = "plot_sim_pm",
                                        label = "Proportion of missing values",
                                        choices = sort(unique(plotmispcr::dataResults$pm)),
                                        selected = sort(unique(plotmispcr::dataResults$pm))[3],
                                        inline = FALSE
                                    ),
                                    checkboxGroupInput("plot_sim_mech",
                                        "Missing data mechanism",
                                        inline = FALSE,
                                        choices = levels(plotmispcr::dataResults$mech),
                                        selected = levels(plotmispcr::dataResults$mech)[2]
                                    ),
                                ),
                                shiny::column(
                                    width = 6,
                                    checkboxGroupInput("plot_sim_method",
                                        "Imputation methods:",
                                        choices = levels(plotmispcr::dataResults$method)[-nlevels(plotmispcr::dataResults$method)],
                                        selected = levels(plotmispcr::dataResults$method)[c(1:4, 8)],
                                        inline = FALSE
                                    ),
                                    shinyWidgets::sliderTextInput(
                                        inputId = "plot_sim_npcs",
                                        label = "Number of PCs used for imputation",
                                        hide_min_max = TRUE,
                                        choices = sort(unique(plotmispcr::dataResults$npcs)),
                                        selected = range(plotmispcr::dataResults$npcs),
                                        grid = TRUE
                                    ),
                                )
                            ),
                            selectInput(
                                inputId = "plot_sim_y_axis",
                                label = "Outcome measure",
                                choices = c("PRB", "CIC", "CIW", "Raw bias"),
                                selected = "PRB"
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    width = 6,
                                    radioButtons(
                                        inputId = "plot_sim_stat",
                                        label = "Statistic",
                                        inline = FALSE,
                                        choices = unique(plotmispcr::dataResults$stat)
                                    ),
                                ),
                                shiny::column(
                                    width = 6,
                                    radioButtons(
                                        inputId = "plot_sim_vars",
                                        label = "Variables",
                                        inline = FALSE,
                                        choices = unique(plotmispcr::dataResults$vars)
                                    ),
                                )
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    width = 6,
                                    shinyWidgets::sliderTextInput(
                                        inputId = "plot_sim_y_range",
                                        label = "Y-axis range",
                                        hide_min_max = FALSE,
                                        choices = 0:100,
                                        selected = c(0, 60),
                                        grid = FALSE
                                    ),
                                ),
                                shiny::column(
                                    width = 6,
                                    shiny::sliderInput(
                                        inputId = "plot_sim_point_size",
                                        label = "Point size",
                                        min = 0,
                                        max = 2,
                                        step = 1,
                                        value = 2
                                    )
                                )
                            ),
                            style = "border-right: 1px solid; border-right-color: #DDDDDD"
                        ),
                        shiny::column(
                            width = 8,
                            shiny::fluidRow(
                                shiny::plotOutput("plot"),

                                # Silent extraction of size
                                shinybrowser::detect(),
                            )
                        )
                    )
                ),
                shiny::tabPanel(
                    title = "Module 2: Convergence checks",
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
                            radioButtons(
                                inputId = "plot_case_method",
                                label = "Imputation method:",
                                choices = levels(plotmispcr::dataResults$method)[1:7],
                                selected = levels(plotmispcr::dataResults$method)[1],
                                inline = FALSE
                            ),
                            selectInput(
                                inputId = "plot_case_npcs",
                                label = "Number of PCs used for imputation",
                                choices = sort(unique(plotmispcr::dataResults$npcs))[-1],
                                selected = sort(unique(plotmispcr::dataResults$npcs))[2]
                            ),
                            shinyWidgets::sliderTextInput(
                                inputId = "plot_case_iters",
                                label = "Iteration range",
                                hide_min_max = TRUE,
                                choices = 0:100,
                                selected = c(0, 100),
                                grid = FALSE
                            ),
                            style = "border-right: 1px solid; border-right-color: #DDDDDD"
                        ),
                        shiny::column(
                            width = 8,
                            shiny::fluidRow(
                                shiny::plotOutput("plot_mids"),

                                # Silent extraction of size
                                shinybrowser::detect(),
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