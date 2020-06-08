
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nettskjemar <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/LCBC-UiO/nettskjemar.svg?branch=master)](https://travis-ci.org/LCBC-UiO/nettskjemar)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/nettskjemar/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/nettskjemar?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/nettskjemar)](https://CRAN.R-project.org/package=nettskjemar)
<!-- badges: end -->

The goal of {nettskjemar} is to have easy access to data and form
information on the [Nettskjema](https://nettskjema.uio.no/) service by
the [University of Oslo, Norway](https://www.uio.no/english/). The
package is under development, and is still lacking alot of desired
functionality. Currently the main possibilities is help in creating
users and tokens for accessing Nettskjema through its API, downloading
meta-data about specific forms and data associated with a specific form.
Not all types of form elements are supported yet.

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/nettskjemar")
```

## Setting up the authentication

There is a tutorial on [how to create the API access user and
token](https://lcbc-uio.github.io/nettskjemar/articles/auth_setup.html)
needed to work with the api. This will need to be completed before you
can access any other features of this package.

## Example

Currently there are very few functions actually working in the package.
The most important, is the function that will download nettskjema
submission data and return them as a tibble (data.frame). This needs
only the *id* of a nettskjema, which can be found in the last part of
the nettskjema url.

``` r
library(nettskjema)

nettskjema_get_data(nettskjema_id)
```

If you do not have the codebook activated, or you want to use the full
answers to questions rather than the coded ones, you can toggle off code
book answers:

``` r
nettskjema_get_data(nettskjema_id, use_codebook = FALSE)
```

## Documentation

Package documentation can be found on the associated [GitHub
pages](https://lcbc-uio.github.io/nettskjemar/), among other
documentation on [how to create the API acces user and
token](https://lcbc-uio.github.io/nettskjemar/articles/auth_setup.html).
