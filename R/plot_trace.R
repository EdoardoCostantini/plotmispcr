#' Trace plots
#'
#' Generate trace plots to study the convergence of imputation methods
#'
#' @param mids_data object containing imputation history produced by the simulation study
#' @param method unit character vector naming the method to display
#' @param npcs integer defining the number of pcs to consider for the PCA-based procedures
#' @param iters numeric vector containing the iteration bounds (from-to) to plot
#' @return Returns the lattice plot
#' @author Edoardo Costantini, 2023
#' @examples
#' # Define example inputs
#' mids_data <- dataMids
#' method <- "pcr"
#' npcs <- 2
#' iters <- c(0, 25)
#' 
#' # Use the function
#' plot_trace(
#'     mids_data = dataMids,
#'     method = "pcr",
#'     npcs = 2,
#'     iters = c(0, 25)
#' )
#' 
#' @export
plot_trace <- function(mids_data, method, npcs, iters = c(0, 25)) {

    # Define condition to plot based on inputs
    cnd_search <- paste0(
        "npcs-", npcs,
        "-method-", method
    )
    cnd_id <- grep(cnd_search, names(plotmispcr::dataMids$mids))

    # Work with simple object name
    x <- plotmispcr::dataMids$mids[[cnd_id]]

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
    formula <- stats::as.formula(paste0(
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
        x = formula, 
        data = data, 
        groups = .m,
        type = type, 
        lty = lty, 
        col = col, 
        layout = layout,
        scales = list(
            y = list(relation = "free"),
            x = list(alternating = FALSE)
        ),
        as.table = TRUE,
        xlim = c(iters[1] - 1, iters[2] + 1),
        xlab = "Iteration",
        ylab = "",
        strip = strip.combined,
        par.strip.text = list(lines = 0.5)
    )
}