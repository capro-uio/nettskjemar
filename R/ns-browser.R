#' Access Nettskjema API Documentation
#'
#' Opens the browser to display the API documentation
#' for the Nettskjema PI.
#'
#' @return None. Opens a browser window.
#' @export
#' @examples
#' \dontrun{
#' ns_api_docs()
#' }
ns_api_docs <- function() {
  open_browser("https://api.nettskjema.no/v3/swagger-ui/index.html")
}


#' Create a Nettskjema Client
#'
#' Opens the Nettskjema client creation URL in the default web browser.
#'
#' Use this function to create a new client for accessing Nettskjema APIs.
#'
#' @return NULL. Opens the client creation page in a browser.
#' @export
#' @examples
#' \dontrun{
#' ns_create_client()
#' }
ns_create_client <- function() {
  open_browser("https://authorization.nettskjema.no/client")
}

#' Open browser if interactive
#' @noRd
open_browser <- function(url) {
  if (interactive()) utils::browseURL(url)
}
