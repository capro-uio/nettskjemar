# *Required* as vcr is set up on loading
library("vcr")

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "<<CLIENT_SECRET>>" = Sys.getenv("NETTSKJEMA_CLIENT_SECRET"),
    "<<BASIC_AUTH>>" = openssl::base64_encode(paste0(
      Sys.getenv("NETTSKJEMA_CLIENT_ID"),
      ":",
      Sys.getenv("NETTSKJEMA_CLIENT_SECRET")
    )),
    "<<ACCESS_TOKEN>>" = ns_auth_token()$access_token
  ),
  dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()

form_id <- 123823
submission_id <- 27685292
attachment_id <- 538819

skip_if_no_auth <- function(skip = TRUE) {
  if (ns_has_auth(verbose = FALSE)) {
    testthat::skip("No authentication available")
  }
}
