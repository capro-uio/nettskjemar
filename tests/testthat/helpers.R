# *Required* as vcr is set up on loading
library("vcr")

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "<<CLIENT_SECRET>>" = Sys.getenv("NETTSKJEMA_CLIENT_SECRET"),
    "<<BASIC_AUTH>>" = openssl::base64_encode(
      paste0(
        Sys.getenv("NETTSKJEMA_CLIENT_ID"),
        ":",
        Sys.getenv("NETTSKJEMA_CLIENT_SECRET")
      )
    )
  ),
  dir = vcr::vcr_test_path("fixtures")
))

#' Helper function to mock Nettskjema authentication for VCR tests
#'
#' This function sets up mocks for Sys.getenv and ns_has_auth
#' to allow VCR to correctly intercept and playback requests
#' without requiring actual Nettskjema credentials.
with_mocked_nettskjema_auth <- function(expr) {
  testthat::local_mocked_bindings(
    ns_has_auth = function(...) TRUE,
    .package = "nettskjemar"
  )
  force(expr)
}

vcr::check_cassette_names()

form_id <- 123823
submission_id <- 27685292
attachment_id <- 538819
