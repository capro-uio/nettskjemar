#' @keywords internal
"_PACKAGE"

## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "value"
  ))
}

# nocov start
release_bullets <- function() {
  c(
    sprintf(
      "Coverage is at: %s",
      covr::coverage_to_list(covr::package_coverage())$totalcoverage
    ),
    knit_vignettes()
  )
} # nocov end

# nocov start
knit_vignettes <- function() {
  proc <- list.files(
    "vignettes",
    "orig$",
    full.names = TRUE
  )

  k <- lapply(proc, function(x) {
    fig_path <- "static"
    knitr::knit(
      x,
      gsub("\\.orig$", "", x)
    )
    imgs <- list.files(fig_path, full.names = TRUE)
    sapply(imgs, function(x) {
      file.copy(
        x,
        file.path("vignettes", fig_path, basename(x)),
        overwrite = TRUE
      )
    })
    invisible(unlink(fig_path, recursive = TRUE))
  })

  list(
    "Knit vignettes",
    lapply(proc, basename)
  )
} # nocov end
