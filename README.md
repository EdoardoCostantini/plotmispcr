# Shiny app: plot mi-spcr results

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7451802.svg)](https://doi.org/10.5281/zenodo.7451802)

An interactive dashboard to interact with the results of a research paper.

## Code Example

To start the shiny apps and interact with the plots, open an R session and load the package:

```
library("plot.mi.scpr")
```

Then, run the following command in the R console:

```
plotResults()
```

You can interact with the the shiny app also [online](https://edoardocostantini.shinyapps.io/plotmispcr).

## Motivation

This Shiny app allows you to interact with the results of the research paper associated with the GitHub project: [Multiple imputation with the use of supervised principal component regression as a univariate imputation method](https://github.com/EdoardoCostantini/mi-spcr).

## Installation

You can install the Shiny app as an R package by using one of the methods described below.

### Install from GitHub

1. Open an R session.

2. Install `devtools`, an R package that provides a way to install other R packages directly from GitHub. Run the following R command in the console:

    ```
    install.packages("devtools")
    ```

3. Run the following command:

    ```
    devtools::install_github("https://github.com/EdoardoCostantini/plotmispcr")
    ```

### Install from a local folder

1. Download the package from GitHub or [Zenodo](https://doi.org/10.5281/zenodo.7451802).

2. Unzip the package.

3. Open an R session, and run the following command.

    ```
    install.packages(
        "path to the folder containing the package",
        repos = NULL,
        type = "source"
    )
    ```

    For example, on a Windows computer, this could be

    ```
    install.packages(
        "C:/Users/Name/Downloads/plotmispcr/",
        repos = NULL,
        type = "source"
    )
    ```

## Tests

### Unit tests

**Unit tests** to test whether the functions behave as expected. This tests are performed by using the `test()` fucntion from the `devtools` R package.
The workflow is the one established by the `testthat` R package.
To run these tests with any of these methods:

- R console

   1. Install the `devtools` R package, if you do not have it already.

        ``` r
        install.packages("devtools")
        ```

   2. Then, you can the test with the following command in the R console:

        ``` r
        devtools::test()
        ```

        or 

        ``` r
        devtools::check()
        ```

- Bash terminal

    ```bash
    R CMD check
    ```

## Contributors

- [Edoardo Costantini](https://github.com/EdoardoCostantini)

## License

Copyright (c) 2023 Edoardo Costantini

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.