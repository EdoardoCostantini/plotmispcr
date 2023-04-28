#' server
#'
#' server function for the shiny app
#'
#' @param input set of inputs coming from ui
#' @param output set of outputs
#' @param session session info and status
#' @author Edoardo Costantini, 2023
#' @export
server <- function(input, output, session) {
        # Dynamically update inputs --------------------------------------------

        # > Page width ---------------------------------------------------------
        observe({
            if (shinybrowser::get_width() < 768) {
                updateCheckboxGroupInput(session,
                    inputId = "mech",
                    selected = levels(dataResults$mech)[2]
                )
            }
        })

        # > Statistics and Variables requested ---------------------------------
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

        # > Zoom on the y-axis -------------------------------------------------
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

        # > NPCs displayed by slider based on nla condition --------------------
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

        # Simulation study output ----------------------------------------------
        output$plot <- renderPlot(
            res = 96,
            height = 750,
            {
                # Use the function
                plot_simulation(
                    results = dataResults,
                    outcome = input$plot_sim_y_axis,
                    n_latent = input$plot_sim_nla,
                    na_mechanism = input$plot_sim_mech,
                    prop_na = input$plot_sim_pm,
                    variables = input$plot_sim_vars,
                    parameter = input$plot_sim_stat,
                    method_vector = input$plot_sim_method,
                    npc_range = input$plot_sim_npcs,
                    y_axis_range = input$plot_sim_y_range
                )
            }
        )

        # Convergence checks plot ----------------------------------------------
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