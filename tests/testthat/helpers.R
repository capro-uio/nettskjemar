skip_if_no_auth <- function(token_name = "NETTSKJEMA_API_TOKEN") {
  if (identical(Sys.getenv(token_name), "")) {
    testthat::skip("No authentication available")
  }
}
