# *Required* as vcr is set up on loading
library("vcr")

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "<<CLIENT_SECRET>>" = Sys.getenv("NETTSKJEMA_CLIENT_SECRET")
  ),
  dir = vcr::vcr_test_path("_vcr")
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

form_id <- 123823
submission_id <- 27685292
attachment_id <- 538819

# nolint start
mock_client_id = "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
mock_client_secret = "aB3xK9mP2vQ8fR7nL98Mcs81sT4uY6wE5zC0hJ9iO3kM8pN2qA7bD1gF4jH6lS9vX3nR5mT8"
# nolint end
