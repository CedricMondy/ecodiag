
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecodiag

The `ecodiag` package allows to build ecological Diagnostic Tools (DT)
in a standardized way. A DT corresponds to a set of random forest models
(built using the `ranger` and `mlr` packages) designed to predict the
probability of an ecological community being impacted by different
driving factors (one model per driving factor).

Still in development.

## Installation

You can install ecodiag from github with:

``` r
# install.packages("devtools")
devtools::install_github("CedricMondy/ecodiag")
```

## Example

For examples, see the vignette `workflow`.
