
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nettskjemar <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/capro-uio/nettskjemar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/capro-uio/nettskjemar/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/206264675.svg)](https://zenodo.org/badge/latestdoi/206264675)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/nettskjemar)](https://CRAN.R-project.org/package=nettskjemar)
[![Codecov test
coverage](https://codecov.io/gh/capro-uio/nettskjemar/graph/badge.svg)](https://app.codecov.io/gh/capro-uio/nettskjemar)
<!-- badges: end -->

The goal of {nettskjemar} is to have easy access to data and form
information on the [Nettskjema](https://nettskjema.no/) service by the
[University of Oslo, Norway](https://www.uio.no/english/). Currently the
main possibilities is help in creating users and tokens for accessing
Nettskjema through its API, downloading meta-data and codebooks about
specific forms and data associated with a specific form.

Install the released version from CRAN with:

``` r
install.packages("nettskjemar")
```

Newer releases can be installed through the authors r-universe:

``` r
# Enable this universe
options(repos = c(
    lcbc_uio = 'https://CAPRO-UIO.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('nettskjemar')
```

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("CAPRO-UIO/nettskjemar")
```

## Example

Currently, the package contains functions to download data from forms,
including submission answers, meta-data, and codebook information. The
most important, is the function that will download nettskjema submission
data and return them as a data.frame. This needs only the *id* of a
nettskjema.

``` r
library(nettskjemar)

nettskjema_get_data(nettskjema_id)
```

## Documentation

Package documentation can be found on the associated [GitHub
pages](https://www.capro.dev/nettskjemar/), where there are 6 tutorials.

# Citation

To cite nettskjemar in publications use:

Athanasia Monika Mowinckel. (2021, May 10). LCBC-UiO/nettskjemar:
v0.1.04 (Version v0.1.04). Zenodo.
<http://doi.org/10.5281/zenodo.4745481>

A BibTeX entry for LaTeX users is

@Manual{, title = {nettskjemar - Package to work with University of Oslo
Nettskjema API}, author = {Athanasia Mo Mowinckel}, year = {2021}, month
= {05}, day = {10}, doi = {10.5281/zenodo.4745481}, }

# Funding

This tool is partly funded by:

**EU Horizon 2020 Grant:** Healthy minds 0-100 years: Optimising the use
of European brain imaging cohorts (Lifebrain).

**Grant agreement number:** 732592.

**Call:** Societal challenges: Health, demographic change and well-being
