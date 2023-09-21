# Project:   plotmispcr
# Objective: Test the plot trace function works as expected
# Author:    Edoardo Costantini
# Created:   2023-09-21
# Modified:  2023-09-21
# Notes:

# Run the function
plot_result <- plot_trace(
    mids_data = dataMids,
    method = "pcr",
    npcs = 2,
    iters = c(0, 25)
)

# Test it reports the right outcome
testthat::expect_equal(
    class(plot_result),
    c("trellis")
)
