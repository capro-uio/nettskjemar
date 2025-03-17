#' Information about the current user
#'
#' Based on login credential, returns
#' information on the user accessing the
#' Nettskjema PI
#'
#' @return list of information
#' @export
#' @examples
#' \dontrun{
#' ns_get_me()
#' }
ns_get_me <- function() {
  ns_req() |>
    httr2::req_url_path_append("me") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
