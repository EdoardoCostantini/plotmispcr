# Project:   plotmispcr
# Objective: Prepare data for shiny app
# Author:    Edoardo Costantini
# Created:   2024-03-02
# Modified:  2024-03-02
# Notes: 

# Data to plot
dataResults <- readRDS("./data-raw/20221202-105949-results.rds")
dataMids <- readRDS("./data-raw/20220729-104900-convergence-check.rds")

# Make names of outcome variables prettier
names(dataResults)[names(dataResults) == "coverage"] <- "CIC"
names(dataResults)[names(dataResults) == "CIW_avg"] <- "CIW"

# Change name of imputation methods
levels(dataResults$method) <- c(
    "MI-PCR", "MI-SPCR", "MI-PLSR", "MI-PCovR",
    "MI-QP", "MI-AM", "MI-ALL", "CC", "Fully observed"
)

# Change name of imputation methods
levels(dataResults$method) <- c(
    "MI-PCR", "MI-SPCR", "MI-PLSR", "MI-PCovR",
    "MI-QP", "MI-AM", "MI-ALL", "CC", "Fully observed"
)

# Change statistics names
dataResults$stat <- factor(
    x = dataResults$stat,
    levels = unique(dataResults$stat),
    labels = c("Correlation", "Covariance", "Mean", "Variance")
)

# Change name or raw bias
colnames(dataResults)[colnames(dataResults) == "RB"] <- "Raw bias"

# Change names of methods in the condition tags for the convergence checks
names(dataMids$mids) <- gsub("-pcr-", "-MI-PCR-", names(dataMids$mids))
names(dataMids$mids) <- gsub("-spcr-", "-MI-SPCR-", names(dataMids$mids))
names(dataMids$mids) <- gsub("-pls-", "-MI-PLSR-", names(dataMids$mids))
names(dataMids$mids) <- gsub("-pcovr-", "-MI-PCovR-", names(dataMids$mids))
names(dataMids$mids) <- gsub("-qp-", "-MI-QP-", names(dataMids$mids))
names(dataMids$mids) <- gsub("-am-", "-MI-AM-", names(dataMids$mids))
names(dataMids$mids) <- gsub("-all-", "-MI-ALL-", names(dataMids$mids))

# Change the names also in the condition table
levels(dataMids$cnds$method) <- levels(dataResults$method)
dataMids$cnds$tag <- gsub("-pcr-", "-MI-PCR-", dataMids$cnds$tag)
dataMids$cnds$tag <- gsub("-spcr-", "-MI-SPCR-", dataMids$cnds$tag)
dataMids$cnds$tag <- gsub("-pls-", "-MI-PLSR-", dataMids$cnds$tag)
dataMids$cnds$tag <- gsub("-pcovr-", "-MI-PCovR-", dataMids$cnds$tag)
dataMids$cnds$tag <- gsub("-qp-", "-MI-QP-", dataMids$cnds$tag)
dataMids$cnds$tag <- gsub("-am-", "-MI-AM-", dataMids$cnds$tag)
dataMids$cnds$tag <- gsub("-all-", "-MI-ALL-", dataMids$cnds$tag)

# Manually compress the mids objects
for (i in 1:length(dataMids$mids)) {
    # Keep the only two objects you need for the trace plots
    dataMids$mids[[i]] <- list(
        chainMean = dataMids$mids[[i]]$chainMean,
        chainVar = dataMids$mids[[i]]$chainVar,
        m = dataMids$mids[[i]]$m,
        iteration = dataMids$mids[[i]]$iteration
    )

    # Get rid of non-imputed values
    dataMids$mids[[i]]$chainMean <- dataMids$mids[[i]]$chainMean[1:3, , ]
    dataMids$mids[[i]]$chainVar <- dataMids$mids[[i]]$chainVar[1:3, , ]
}

# Save the two objects as .rda ready for shiny app
usethis::use_data(dataResults, overwrite = TRUE)
usethis::use_data(dataMids, overwrite = TRUE)