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
#' @param ... arguments passed to \code{ns_req_auth}
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
    ns_req_auth()
}

#' Set general nettskjema api URL.
#' @noRd
ns_url <- function() {
  "https://nettskjema.no/api/v3/"
}

#' Authenticate Nettskjema request
#'
#' After creating a client in Nettskjema,
#' this function will retrieve the access
#' token needed for the remaining processes
#' in the package. Automatically caches the
#' token for more efficient API usage.
#'
#' @param req An httr2 request, usually {\code{\link{ns_req}}}
#' @param client_id Character. Retrieved from the
#'     Client portal.
#' @param client_secret Character. Retrieved from the
#'     Client portal.
#' @param client_name Character. Used to identify who
#'     has been running the commands.
ns_req_auth <- function(
  req,
  client_id = Sys.getenv("NETTSKJEMA_CLIENT_ID"),
  client_secret = Sys.getenv("NETTSKJEMA_CLIENT_SECRET"),
  client_name = "nettskjemar"
) {
  httr2::req_oauth_client_credentials(
    req,
    client = ns_client(
      client_id = client_id,
      client_secret = client_secret,
      client_name = client_name
    )
  )
}

#' Create an OAuth2 Client for Nettskjema API
#'
#' This function initializes an OAuth2 client using
#'  the `httr2::oauth_client` function. It is used to
#' authenticate and interact with the Nettskjema API.
#'
#' @param client_id [character] The client ID provided by Nettskjema.
#' @param client_secret [character] The client secret provided
#'     by Nettskjema.
#' @param client_name [character] An optional name for the
#'     client (default = "nettskjemar").
#'
#' @return A configured `httr2::oauth_client` object.
#'
#' @examples
#' # Example: Initialize an OAuth2 client for Nettskjema
#' client <- ns_client(
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret"
#' )
#'
#' # Using a custom client name
#' client <- ns_client(
#'   client_id = "your_client_id",
#'   client_secret = "your_client_secret",
#'   client_name = "custom_client_name"
#' )
#'
#' @export
ns_client <- function(
  client_id,
  client_secret,
  client_name = "nettskjemar"
) {
  # Check for valid id and secret
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

  httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    name = client_name,
    token_url = "https://authorization.nettskjema.no/oauth2/token",
    auth = "header"
  )
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
#' @inheritParams ns_client
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
