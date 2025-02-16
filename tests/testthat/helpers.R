skip_if_no_auth <- function() {
  if (identical(Sys.getenv("NETTSKJEMA_CLIENT_SECRET"), "")) {
    testthat::skip("No authentication available")
  }
}