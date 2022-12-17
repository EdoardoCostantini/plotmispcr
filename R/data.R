# Project:   plot.mi.spcr
# Objective: container for data documentation
# Author:    Edoardo Costantini
# Created:   2022-12-01
# Modified:  2022-12-16

#' dataResults
#' 
#' The data.frame containing the results of the simulation study. It is automatically called by the plotting function.
#'
#' The columns of the data.frame are
#'
#' \itemize{
#'   \item tag. Character vector describing the condition
#'   \item npcs. Number of principal components used
#'   \item method. Imputation method used
#'   \item nla. Number of latent variables used to generate the data
#'   \item auxcor. Correlation between the main latent variables and the auxiliary ones
#'   \item pm. Proportion of missing cases on each variable with missing values
#'   \item mech. Missing data mechanism
#'   \item loc. Location of the missing data in the variable distribution
#'   \item p. Total number of items
#'   \item stat. Statistics computed
#'   \item vars. Variables involved in the statistic
#'   \item est_avg. Average estimate of the statistic over the Monte Carlo repetitions
#'   \item mcsd. Standard deviation of the statistic estimate over the Monte Carlo repetitions
#'   \item ref. True value of the statistic
#'   \item RB. Raw bias
#'   \item PRB. Percent relative bias
#'   \item CIC. Confidence interval coverage
#'   \item CIW. Average confidence interval width
#'   \item CIW_sd. Standard deviation of the confidence interval width
#'   \item CIW_lwr_avg. Average value of the confidence interval lower bound
#'   \item CIW_upr_avg. Average value of the confidence interval upper bound
#' }
#'
#' @docType data
#' @keywords datasets
#' @name dataResults
#' @usage data(dataResults)
#' @format A data frame with 12108 rows and 21 columns
NULL

#' dataMids
#' 
#' A list of (simplified) mids objects and condition descriptions used to obtain the trace plots for desired conditions.
#'
#' @docType data
#' @keywords datasets
#' @name dataMids
#' @usage data(dataMids)
#' @format A list containing (1) simplified mids for 81 conditions described in a (2) data.frame
NULL