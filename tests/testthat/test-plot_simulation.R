# Project:   plotmispcr
# Objective: Test the plot simulation function works as expected
# Author:    Edoardo Costantini
# Created:   2023-09-21
# Modified:  2023-09-21
# Notes:

# Run the function
plot_result <- plot_simulation(
    results = dataResults,
    outcome = "PRB",
    n_latent = dataResults$nla[2],
    na_mechanism = levels(dataResults$mech),
    prop_na = sort(unique(dataResults$pm)),
    variables = unique(dataResults$vars)[1],
    parameter = unique(dataResults$stat)[1],
    method_vector = levels(dataResults$method)[1:4],
    npc_range = c(0, 12),
    y_axis_range = c(0, 60)
)

# Test it reports the right outcome
testthat::expect_equal(
    class(plot_result),
    c("gg", "ggplot")
)
