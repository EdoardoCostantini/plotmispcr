# Shiny app: plot mi-spcr results

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7451802.svg)](https://doi.org/10.5281/zenodo.7451802)

This is a repository to host the `ui.R` and `server.R` and input file required by a shiny dashboard to dynamically sort through the results of the simulation study performed [here](https://github.com/EdoardoCostantini/mi-spcr)

You can interact with the results from the study [mi-spcr](https://github.com/EdoardoCostantini/mi-spcr) by:

- Visit the online [shiny app](https://edoardocostantini.shinyapps.io/plotmispcr) to check out the simulation study results.
- Installing this app locally as an R package to check the convergence plot as well:

    ```
    devtools::install_github("https://github.com/EdoardoCostantini/plotmispcr")
    ```

## Plots

To start the shiny apps and interact with the plots, open an R session and load the package:

```
library("plotmipca")
```

Then, run the following command in the R console:

```
plotResults()
```

You can plot the results for the values of the following simulation study experimental factors:

- Number of latent variables used to generate the data (2, 10, 50)
- Proportion of missing values imposed on every variable (0.1, 0.25, 0.5)
- Missing data mechanism imposed (MCAR, MAR)
- Imputation methods (see names in the interface)

    - pcr: mice with principal component regression as univariate imputation method
    - spcr: mice with supervised principal component regression as univariate imputation method
    - plsr: mice with partial least squares regression as univariate imputation method
    - pcovr: mice with principal covariates regression as univariate imputation method
    - qp: mice with normal linear model with bootstrap as univariate imputation method and quickpred() used to select the predictors
    - am: mice with normal linear model with bootstrap as univariate imputation method and the analysis model variables used as predictors
    - all: mice with normal linear model with bootstrap as univariate imputation method and all available items used as predictors
    - cc: complete case analysis
    - fo: fully observed data (results if there had been no missing values)

- Number of principal components (depending on the number of latent variables used)
- Outcome measure:

    - RB: raw bias
    - PRB: percent relative bias
    - CIC: confidence interval coverage
    - CIW: average confidence interval
    - mcsd: Standard deviation of the estimate across the monte carlo simulations

- Statistic:

    - cor: correlation between two items with missing values
    - cov: covariance between two items with missing values
    - cor: mean of an item with missing values
    - cor: variance of an item with missing values

- Variables considered
- Zoom on the y-axis