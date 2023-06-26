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
        shiny::withMathJax(),
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
                            <a href=''>Costantini, E., Lang, K. M., Sijtsma, K., & Reeskens, T. (2022). Supervised dimensionality reduction for multiple imputation by chained equations. <i>coming soon</i>.</a>
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
                            shiny::navlistPanel(
                                widths = c(11, 12),
                                shiny::tabPanel(
                                    title = "1. Data generation",
                                    shiny::HTML(
                                        "<br>
                                            Here you can change how the data were generated.
                                            <br>
                                            <br>"
                                    ),
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
                                    title = "2. Missing data treatments",
                                    shiny::HTML(
                                        "<br>
                                            Here you can change which missing data treatments to show.
                                            <br>
                                            <br>"
                                    ),
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
                                ),
                                shiny::tabPanel(
                                    title = "3. Simulation outcomes",
                                    shiny::HTML(
                                        "<br>
                                            Here you can change what outcome measures to plot.
                                            <br>
                                            <br>"
                                    ),
                                    selectInput(
                                        inputId = "plot_sim_y_axis",
                                        label = "Outcome measure",
                                        choices = c("RB", "PRB", "CIC", "CIW", "mcsd"),
                                        selected = "PRB"
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
                                )
                            )
                        ),
                        shiny::column(
                            width = 8,
                            shiny::fluidRow(
                                shiny::plotOutput("plot"),

                                # Silent extraction of size
                                shinybrowser::detect(),
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
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
                                selected = c(0, 100),
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
                ),
                shiny::tabPanel(
                    title = "Supplementary material",
                    shiny::fluidRow(
                        shiny::column(
                            width = 4,
                            shiny::HTML(
                                "An interesting similarity between the SDR methods emerged.
                The more components are used, the closer their performance becomes to that of MI-PCR.
                Both MI-SPCR and MI-PCovR approaches seemed to perform best when using as few components as possible.
                The more components were used, the higher the bias.
                This result is due to a common characteristic between the two approaches.
                The more components we ask for, the less supervision the two methods can exercise when computing the components.
                The more components we want, the lower the importance of supervision in defining the components.
                In MI-SPCR, the cause of this result is the threshold value \\(\\pi\\), while in MI-PCovR, the cause lies in the choice of $$\\alpha_{ML} \\leq %.03f ) = %.03f$$"
                    ),
                            shiny::tabsetPanel(
                                shiny::tabPanel(
                                    title = "A",
                                    shiny::HTML(
                                        "An irrational number \\(\\sqrt{2}\\)
                                            and a fraction $$1-\\frac{1}{2}$$. Can I add a \\pi"
                                            ),
                                    shiny::HTML("If \\(X\\) is a Cauchy random variable, then
                    $$P(X \\leq %.03f ) = %.03f$$")
                                ),
                                shiny::tabPanel(
                                    title = "B",
                                    shiny::HTML("and a fact about \\(\\pi\\):
           $$\\frac2\\pi = \\frac{\\sqrt2}2 \\cdot
           \\frac{\\sqrt{2+\\sqrt2}}2 \\cdot
           \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2 \\cdots$$")
                                ),
                            )
                        ),
                        shiny::column(
                            width = 8,
                            shiny::fluidRow(
                                shiny::plotOutput("plot_sup_npcs"),

                                # Silent extraction of size
                                shinybrowser::detect(),
                            ),
                            style = "border-left: 1px solid; border-left-color: #DDDDDD"
                        )
                    ),
                    shiny::fluidRow(
                        shiny::column(
                            width = 4,
                            shiny::HTML(
                                "<br>
                                            Here you can change how the data were generated.
                                            <br>
                                            <br>"
                            ),
                            radioButtons(
                                inputId = "plot_many_pcs_nla",
                                label = "Number of latent variables",
                                choices = sort(unique(dataResults$nla)),
                                selected = sort(unique(dataResults$nla))[2],
                                inline = TRUE
                            ),
                            radioButtons(
                                inputId = "plot_many_pcs_pm",
                                label = "Proportion of missing values",
                                choices = sort(unique(dataResults$pm)),
                                selected = sort(unique(dataResults$pm))[3],
                                inline = TRUE
                            ),
                            radioButtons("plot_many_pcs_mech",
                                "Missing data mechanism",
                                choices = levels(dataResults$mech),
                                selected = levels(dataResults$mech)[2],
                                inline = TRUE
                            )
                        ),
                        shiny::column(
                            width = 4,
                            shiny::HTML(
                                "<br>
                                Here you can change which missing data treatments to show.
                                <br>
                                <br>"
                            ),
                            checkboxGroupInput("plot_many_pcs_method",
                                "Imputation methods to compare:",
                                choices = levels(dataResults$method)[c(1:4)],
                                selected = levels(dataResults$method)[c(1:2, 4)],
                                inline = TRUE
                            ),
                            shinyWidgets::sliderTextInput(
                                inputId = "plot_many_pcs_npcs",
                                label = "Number of principal components",
                                hide_min_max = TRUE,
                                choices = sort(unique(dataResults$npcs)),
                                selected = range(dataResults$npcs),
                                grid = TRUE
                            )
                        ),
                        shiny::column(
                            width = 4,
                            shiny::HTML(
                                "<br>
                                            Here you can change what outcome measures to plot.
                                            <br>
                                            <br>"
                            ),
                            selectInput(
                                inputId = "plot_many_pcs_y_axis",
                                label = "Outcome measure",
                                choices = c("PRB", "CIC"),
                                selected = "PRB"
                            ),
                            radioButtons(
                                inputId = "plot_many_pcs_stat",
                                label = "Statistic",
                                inline = TRUE,
                                choices = unique(dataResults$stat)
                            ),
                            radioButtons(
                                inputId = "plot_many_pcs_vars",
                                label = "Variables",
                                inline = TRUE,
                                choices = unique(dataResults$vars)
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