#' Plot many pcs simulation results
#'
#' Generate a plot to compare the performance trends for wide ranges of npcs.
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
#' @return Returns the ggplot
#' @author Edoardo Costantini, 2023
#' @examples
#' # Define example inputs
#' results <- dataResults
#' outcome <- "PRB"
#' n_latent <- unique(dataResults$nla)[3]
#' na_mechanism <- levels(dataResults$mech)[1]
#' prop_na <- sort(unique(dataResults$pm))[3]
#' variables <- unique(dataResults$vars)[1]
#' parameter <- unique(dataResults$stat)[1]
#' method_vector <- levels(dataResults$method)[1:4]
#' npc_range <- c(0, 12)
#' y_axis_range <- c(0, 60)
#'
#' # Use the function
#' plot_many_pcs(
#'     results = dataResults,
#'     outcome = "PRB",
#'     n_latent = dataResults$nla[2],
#'     na_mechanism = levels(dataResults$mech),
#'     prop_na = sort(unique(dataResults$pm)),
#'     variables = unique(dataResults$vars)[1],
#'     parameter = unique(dataResults$stat)[1],
#'     method_vector = levels(dataResults$method)[1:4],
#'     npc_range = c(0, 149),
#'     y_axis_range = c(0, 60)
#' )
#'
#' @export
plot_many_pcs <- function(results, outcome, y_axis_range, parameter, variables, n_latent, na_mechanism, prop_na, method_vector, npc_range) {
    
    # Filter the data as requested
    results_filtered <- results %>%
        filter(
            .data$nla == n_latent,
            .data$mech == na_mechanism,
            .data$pm == prop_na,
            .data$vars == variables,
            .data$stat == parameter,
            .data$method %in% method_vector,
            .data$npcs <= npc_range[2],
            .data$npcs >= npc_range[1]
        )

    # Make plot
    results_filtered %>%
        ggplot(aes(
            x = .data[["npcs"]],
            y = .data[[outcome]],
            group = .data[["method"]]
        )) +
        # geom_point(color = "darkgray") +
        geom_line(
            aes(
                linetype = .data[["method"]],
                color = .data[["method"]]
            ),
            linewidth = .6
        ) +
        # scale_x_continuous(
        #     breaks = sort(unique(results$npcs)),
        #     sort(unique(results$npcs))
        # ) +
        scale_color_manual(
            values = c(
                "spcr" = "black",
                "pcovr" = "black",
                "plsr" = "black",
                "pcr" = "darkgray"
            )
        ) +
        scale_linetype_manual(
            values = c(
                "spcr" = "solid",
                "pcovr" = "longdash",
                "plsr" = "dotted",
                "pcr" = "solid"
            )
        ) +

        # Legend
        guides(
            linetype = guide_legend(
                keywidth = 5,
                keyheight = 1,
                label.position = "bottom",
                reverse = TRUE
            ),
            color = guide_legend(
                keywidth = 5,
                keyheight = 1,
                label.position = "bottom",
                reverse = TRUE
            )
        ) +

        # Theme
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