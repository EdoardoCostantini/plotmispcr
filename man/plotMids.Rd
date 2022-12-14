% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{plotMids}
\alias{plotMids}
\title{plotMids}
\usage{
plotMids()
}
\value{
Shiny app UI.
}
\description{
Starts a Shiny app to check trace plots for multiple imputation convergence for the \href{https://github.com/EdoardoCostantini/mi-spcr}{mi-spcr} project.
}
\details{
The interface of the Shiny app allows you to change the values of the following simulation study experimental factors:
\itemize{
\item Missing data treatment used (see names in the interface)
\itemize{
\item pcr: mice with principal component regression as univariate imputation method
\item spcr: mice with supervised principal component regression as univariate imputation method
\item plsr: mice with partial least squares regression as univariate imputation method
\item pcovr: mice with principal covariates regression as univariate imputation method
\item qp: mice with normal linear model with bootstrap as univariate imputation method and quickpred() used to select the predictors
\item am: mice with normal linear model with bootstrap as univariate imputation method and the analysis model variables used as predictors
\item all: mice with normal linear model with bootstrap as univariate imputation method and all available items used as predictors
}
\item Number of principal components (depending on the number of latent variables used)
\item Number of iterations used
}
}
