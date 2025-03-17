form_id       <- 123823
submission_id <- 27685292
attachment_id <- 841793

skip_if_no_auth <- function() {
  if (identical(Sys.getenv("NETTSKJEMA_CLIENT_SECRET"), "")) {
    testthat::skip("No authentication available")
  }
}
