
#' Check nettskjema API token expiry date
#'
#' Given an API token, will retrieve the expiry
#' date of the said token.
#'
#' @inheritParams nettskjema_get_data
#'
#' @return character date string
#' @export
#' @importFrom httr content
nettskjema_token_expiry <- function(token_name = "NETTSKJEMA_API_TOKEN"){

  # Check that token exists in env
  if(Sys.getenv(token_name) == "")
    stop("Token with name '", token_name, "' does not exist.",
         call. = FALSE)

  resp <- nettskjema_api("users/admin/tokens/expire-date",
                         token_name = token_name)

  api_catch_error(resp)

  dt <- as.Date(unlist(content(resp)))

  message("Token with name '", token_name, "' expires in ",
          as.numeric(dt - Sys.Date()),
          " days."
  )

  invisible(dt)
}


#' Create Nettskjema API user
#'
#' Opens OS browser to create API user.
#' @param ip_version IP version to look up. Either "v4" (default) or "v6".
#' @return No return value, opens a browser for creating a API user.
#' @importFrom utils browseURL
#' @export
#' @examples
#' \dontrun{
#' nettskjema_user_create()
#'
#' # Turn off ip detection
#' nettskjema_user_create(ip = FALSE)
#' }
nettskjema_user_create <- function(ip_version = c("v4", "v6")){
  nettskjema_find_ip(ip_version)
  browseURL("https://nettskjema.uio.no/user/api/index.html")
}


#' nettskjema api connection
#'
#' @param path path connection
#' @template token_name
#' @param ... arguments passed to GET
#'
#' @return an httr response
#' @importFrom httr GET add_headers
#' @noRd
nettskjema_api <- function(path, token_name, ...) {
  url <- paste0("http://nettskjema.no/api/v2/", path)
  GET(url,
      ...,
      add_headers(Authorization = api_auth(token_name))
  )
}

#' Find your current IP
#'
#' The Nettskjema v2 API requires a token
#' to be registered with IP addresses.
#' This function returns the current IP address.
#'
#' If you are working through a VPN, the IP address
#' will not be returned as the Nettskjema API sees it.
#' We are looking for solutions to this.
#'
#' @param version string. ip version to check.
#'   one of either "v4" or "v6".
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @export
#' @examples
#' nettskjema_find_ip()
nettskjema_find_ip <- function(version = c("v4","v6")){
  version <- match.arg(version, c("v4","v6"))
  ip_url <- switch(version,
                   "v4" = "https://api.ipify.org?format=json",
                   "v6" = "https://api64.ipify.org?format=json")
  resp <- GET(ip_url)
  message(
    sprintf(
      "Your current IP%s address is:\n%s",
      version,
      fromJSON(content(resp, "text",
                       encoding = "UTF-8"))$ip
    )
  )
}

# helpers ----
#' @noRd
#' @template token_name
api_auth <- function(token_name = "NETTSKJEMA_API_TOKEN"){
  paste("Bearer", Sys.getenv(token_name))
}

