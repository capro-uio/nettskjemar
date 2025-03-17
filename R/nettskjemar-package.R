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
  # Pre-compiled vignettes that depend on API key
  proc <- list.files(
    "vignettes",
    "orig$",
    full.names = TRUE
  )

  lapply(proc, function(x) {
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
    cat("Updated: ", basename(x), "\n")
  })

  cov <- covr::package_coverage()
  cat(
    "Total coverage: ",
    covr::coverage_to_list(cov)$totalcoverage,
    sep = ""
  )
  #covr::zero_coverage(cov) #nolint
} # nocov end
