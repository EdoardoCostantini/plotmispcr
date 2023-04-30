#' plotResults
#'
#' Starts a Shiny app to interact with the results of the \href{https://github.com/EdoardoCostantini/mi-spcr}{mi-spcr} project.
#'
#' @details
#' The interface of the Shiny app allows you to change the values of the following simulation study experimental factors:
#'
#' - Number of latent variables used to generate the data (2, 10, 50)
#' - Proportion of missing values imposed on every variable (0.1, 0.25, 0.5)
#' - Missing data mechanism imposed (MCAR, MAR)
#' - Imputation methods (see names in the interface)
#'
#'      - pcr: mice with principal component regression as univariate imputation method
#'      - spcr: mice with supervised principal component regression as univariate imputation method
#'      - plsr: mice with partial least squares regression as univariate imputation method
#'      - pcovr: mice with principal covariates regression as univariate imputation method
#'      - qp: mice with normal linear model with bootstrap as univariate imputation method and quickpred() used to select the predictors
#'      - am: mice with normal linear model with bootstrap as univariate imputation method and the analysis model variables used as predictors
#'      - all: mice with normal linear model with bootstrap as univariate imputation method and all available items used as predictors
#'      - cc: complete case analysis
#'      - fo: fully observed data (results if there had been no missing values)
#'
#' - Number of principal components (depending on the number of latent variables used)
#' - Outcome measure:
#'
#'      - RB: raw bias
#'      - PRB: percent relative bias
#'      - CIC: confidence interval coverage
#'      - CIW: average confidence interval
#'      - mcsd: Standard deviation of the estimate across the monte carlo simulations
#'
#' - Statistic:
#'
#'      - cor: correlation between two items with missing values
#'      - cov: covariance between two items with missing values
#'      - cor: mean of an item with missing values
#'      - cor: variance of an item with missing values
#'
#' - Variables considered
#' - Zoom on the y-axis
#'
#' @export
#' @import shiny
#' @import shinybrowser
#' @import dplyr
#' @import ggplot2
#' @import shinyWidgets
#' @import pkgload
#' @return Shiny app UI.
#'
plotResults <- function() {
    shinyApp(
        ui = ui_call(),
        server
    )
}
