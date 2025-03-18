#' Basic Nettskjema httr2 request
#'
#' Sets the API url and access token.
#' This is used for almost all the
#' functionality in this package. If
#' you want to use endpoints not yet
#' covered by functions in the package,
#' this is the basis function you can
#' build from.
#'
#' @param ... arguments passed to \code{ns_auth_token}
#' @return \code{httr2} request
#' @export
#' @examples
#' \dontrun{
#' ns_req() |>
#'  httr2::req_url_path_append("form", "me") |>
#'  httr2::req_perform() |>
#'  httr2::resp_body_json()
#' }
ns_req <- function(...) {
  httr2::request("https://api.nettskjema.no/v3") |>
    httr2::req_auth_bearer_token(
      ns_auth_token(...)$access_token
    )
}

#' Retrieve access token
#'
#' After creating a client in Nettskjema,
#' this function will retrieve the access
#' token needed for the remaining processes
#' in the package. Automatically caches the
#' token for more efficient API usage.
#'
#' @param client_id Character. Default assumes
#'  this is stored in .Renviron as "ns_CLIENT_ID"
#' @param client_secret Character. Default assumes
#'  this is stored in .Renviron as "ns_CLIENT_SECRET"
#' @param cache Logical. Should the token be cached?
#' @param cache_path Character. File path to where
#'   the token should be stored. Defaults to system
#'   cache directory.
ns_auth_token <- function(
  client_id = Sys.getenv("NETTSKJEMA_CLIENT_ID"),
  client_secret = Sys.getenv("NETTSKJEMA_CLIENT_SECRET"),
  cache = TRUE,
  cache_path = NULL
) {
  req <- httr2::request(
    "https://authorization.nettskjema.no/oauth2/token"
  ) |>
    httr2::req_method("POST") |>
    httr2::req_body_raw(
      "grant_type=client_credentials",
      "application/x-www-form-urlencoded"
    ) |>
    httr2::req_auth_basic(
      client_id,
      client_secret
    )

  if (cache) {
    if (is.null(cache_path))
      cache_path <- file.path(
        tools::R_user_dir("nettskjemar", "cache"),
        ".nettskjema-token.rds"
      )
    req <- req |>
      httr2::req_cache(
        cache_path,
        max_age = 24 * 60 * 60
      )
  }

  req |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
