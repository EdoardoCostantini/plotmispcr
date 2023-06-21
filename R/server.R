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

        # Which max?
        if(input$plot_sim_nla == 2){
            default_max_npcs <- max(npcs_to_plot)
        }
        if(input$plot_sim_nla == 10) {
            default_max_npcs <- 12
        }
        if (input$plot_sim_nla == 50) {
            default_max_npcs <- 20
        }

        # Update input slider
        shinyWidgets::updateSliderTextInput(session,
            inputId = "plot_sim_npcs",
            choices = npcs_to_plot,
            selected = c(0, default_max_npcs)
        )
    })

    # > Adjust NPCs option in traceplots based on methods ----------------------
    observe({
        # Subset active data
        active_npcs <- (dataMids$cnds %>%
            filter(method == input$plot_case_method))$npcs

        # Update input
        shiny::updateSelectInput(
            session,
            inputId = "plot_case_npcs",
            choices = sort(active_npcs),
            selected = sort(active_npcs)[1]
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
            # Use the function
            plot_trace(
                mids_data = dataMids,
                method = input$plot_case_method,
                npcs = input$plot_case_npcs,
                iters = input$plot_case_iters
            )
        }
    )

    # Supplementary material plot ----------------------------------------------
    output$plot_sup_npcs <- renderPlot(
        res = 96,
        height = 750,
        {
            # Use the function
            plot_many_pcs(
                results = dataResults,
                outcome = input$plot_many_pcs_y_axis,
                n_latent = input$plot_many_pcs_nla,
                na_mechanism = input$plot_many_pcs_mech,
                prop_na = input$plot_many_pcs_pm,
                variables = input$plot_many_pcs_vars,
                parameter = input$plot_many_pcs_stat,
                method_vector = input$plot_many_pcs_method,
                npc_range = input$plot_many_pcs_npcs,
                y_axis_range = input$plot_many_pcs_y_range
            )
        }
    )
    

}