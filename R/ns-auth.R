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
#'  httr2::req_url_path_append("me") |>
#'  httr2::req_perform() |>
#'  httr2::resp_body_json()
#' }
ns_req <- function(...) {
  httr2::request(ns_url()) |>
    httr2::req_auth_bearer_token(
      ns_auth_token(...)$access_token
    )
}

#' Set general nettskjema api URL.
#' @noRd
ns_url <- function() {
  "https://nettskjema.no/api/v3/"
}

#' Retrieve access token
#'
#' After creating a client in Nettskjema,
#' this function will retrieve the access
#' token needed for the remaining processes
#' in the package. Automatically caches the
#' token for more efficient API usage.
#'
#' @param client_id Character. Retrieved from the Client portal.
#' @param client_secret Character. Retrieved from the Client portal.
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
  if (!ns_has_auth(client_id, client_secret)) {
    cli::cli_abort(c(
      "Variables ",
      "{.code client_id} and ",
      "{.code client_secret} ",
      "are not set up.",
      "Please read ",
      "{.url https://www.capro.dev/nettskjemar/articles/authentication.html}",
      " on how to set your credentials correctly."
    ))
  }

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
    if (is.null(cache_path)) {
      cache_path <- file.path(
        tools::R_user_dir(
          "nettskjemar",
          "cache"
        ),
        client_id
      )
      dir.create(
        dirname(cache_path),
        showWarnings = FALSE,
        recursive = TRUE
      )
    }

    req <- req |>
      httr2::req_cache(
        tempfile(),
        max_age = (24 * 60 * 60) - 1,
        debug = TRUE
      )
  }

  req |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Check Environment Variables for Nettskjema Authentication
#'
#' This function verifies whether the required system
#' variables (`NETTSKJEMA_CLIENT_ID` and
#' `NETTSKJEMA_CLIENT_SECRET`) are set to enable
#' authentication with the Nettskjema API. It provides
#' feedback on the setup status and returns whether the
#' system is correctly configured.
#'
#' @inheritParams ns_auth_token
#'
#' @return Logical. Returns `TRUE` if both environment
#'    variables are set, otherwise `FALSE`.
#'
#' @examples
#' ns_has_auth()
#'
#' @references
#' For more information about authentication setup, see:
#' https://www.capro.dev/nettskjemar/articles/authentication.html
#'
#' @export
ns_has_auth <- function(
  client_id = Sys.getenv("NETTSKJEMA_CLIENT_ID"),
  client_secret = Sys.getenv("NETTSKJEMA_CLIENT_SECRET")
) {
  if (client_id == "" || client_secret == "") return(FALSE)

  TRUE
}
