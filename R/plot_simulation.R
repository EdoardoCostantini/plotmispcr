#' Plot simulation results
#'
#' Generate the main plot for the simulation study.
#'
#' @param results object containing results produced by the simulation study
#' @param outcome performance measure to plot
#' @param y_axis_range range of the outcome measure on the y-axis
#' @param parameter estimated parameter of interest
#' @param variables which variables should be plotted
#' @param n_latent number of latent variables used in the data generating model
#' @param na_mechanism missing data mechanism used to generate the data
#' @param prop_na proportino of missing data generated per-variable
#' @param method_vector experimental factor value: imputation methods considered
#' @param npc_range experimental factor value: number of components considered
#' @param point_size size of the dots in the plot
#' @return Returns the ggplot
#' @author Edoardo Costantini, 2023
#' @examples
#' # Define example inputs
#' results <- dataResults
#' outcome <- "PRB"
#' n_latent <- dataResults$nla[2]
#' na_mechanism <- levels(dataResults$mech)
#' prop_na <- sort(unique(dataResults$pm))
#' variables <- unique(dataResults$vars)[1]
#' parameter <- unique(dataResults$stat)[1]
#' method_vector <- levels(dataResults$method)[1:4]
#' npc_range <- c(0, 12)
#' y_axis_range <- c(0, 60)
#' point_size <- 2.5
#'
#' # Use the function
#' plot_simulation(
#'     results = dataResults,
#'     outcome = "PRB",
#'     n_latent = dataResults$nla[2],
#'     na_mechanism = levels(dataResults$mech),
#'     prop_na = sort(unique(dataResults$pm)),
#'     variables = unique(dataResults$vars)[1],
#'     parameter = unique(dataResults$stat)[1],
#'     method_vector = levels(dataResults$method)[1:4],
#'     npc_range = c(0, 12),
#'     y_axis_range = c(0, 60)
#' )
#'
#' @export
plot_simulation <- function(results, outcome, y_axis_range, parameter, variables, n_latent, na_mechanism, prop_na, method_vector, npc_range, point_size = 2) {
    # Filter the data as requested
    results_filtered <- results %>%
        filter(
            nla == n_latent,
            mech %in% na_mechanism,
            pm %in% prop_na,
            vars == variables,
            stat == parameter,
            method %in% method_vector,
            npcs <= npc_range[2],
            npcs >= npc_range[1]
        )

    # Make plot
    results_filtered %>%
        ggplot(aes(
            x = .data[["npcs"]],
            y = .data[[outcome]],
            group = .data[["method"]]
        )) +
        geom_line(col = "darkgray") +
        geom_point(
            aes(
                shape = .data[["method"]]
            ),
            size = point_size
        ) +
        scale_x_continuous(
            name = "Number of PCs retained",
            breaks = sort(unique(results$npcs)),
            sort(unique(results$npcs))
        ) +

        # Zoomable y-axis
        coord_cartesian(ylim = c(
            y_axis_range[1],
            y_axis_range[2]
        )) +

        # Facet grid
        facet_grid(
            stats::reformulate(
                "mech",
                "pm"
            ),
            labeller = labeller(
                .rows = label_both,
                .cols = label_value
            )
        ) +

        # Theme
        theme(
            # Text
            text = element_text(size = 10),
            axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            # Legend
            legend.title = element_blank(),
            legend.position = "bottom",
            # Backgorund
            panel.background = element_rect(fill = NA, color = "gray")
        )
}