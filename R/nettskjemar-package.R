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
  # Must manually move image files from eia/ to eia/vignettes/ after knit
  proc <- list.files(
    "vignettes",
    "orig$",
    full.names = TRUE
  )

  lapply(proc, function(x) {
    knitr::knit(
      x,
      gsub("\\.orig$", "", x)
    )
    imgs <- list.files("figure", full.names = TRUE)
    sapply(imgs, function(x) {
      file.copy(
        x,
        file.path("vignettes", "static", basename(x)),
        overwrite = TRUE
      )
    })
    invisible(unlink("figure", recursive = TRUE))
  })
} # nocov end
