# Project:   shiny-mi-pca-plot
# Objective: pre-process input data for actual use in shiny app
# Author:    Edoardo Costantini
# Created:   2022-09-13
# Modified:  2022-12-08

# Prepare data -----------------------------------------------------------------

# Data to plot
ggshape <- readRDS("./data/20221202-105949-results.rds")

# Make names of outcome variables prettier
names(ggshape)[names(ggshape) == "coverage"] <- "CIC"
names(ggshape)[names(ggshape) == "CIW_avg"] <- "CIW"

# Shiny dispatch ---------------------------------------------------------------

usethis::use_data(ggshape, overwrite = TRUE)