
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

  dt
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

#' Write token to .Renviron
#'
#' This function will write a token
#' to the .Renviron file with the
#' name you provide.
#'
#' @details Possible actions
#' \itemize{
#'  \item{create}{ - create Renviron token, default}
#'  \item{overwrite}{ - overwrite Renviron token, default}
#'  \item{delete}{ - delete Renviron token, default}
#' }
#'
#' @param token character. Token generated in the UiO portal \code{api_user_create}
#' @param action character. One of three actions: 'create', 'overwrite' or 'delete'.
#' Defaults to 'create'.
#' @template token_name
#'
#' @return No return value. Writes a token to Renviron
#' @export
#' @examples
#' \dontrun{
#' my_token <- "aoiehtvo09e7h"
#' nettskjema_token2renviron(my_token)
#' nettskjema_token2renviron(my_token, action = "overwrite")
#' nettskjema_token2renviron(my_token, action = "delete")
#'
#' # Under a custom name
#' nettskjema_token2renviron(my_token,
#'                          token_name = "NETTAKJEMA_TOKEN_ALT")
#' }
nettskjema_token2renviron <- function(token,
                                      token_name = "NETTSKJEMA_API_TOKEN",
                                      action = c("create", "overwrite", "delete")){
  if(missing(token)) stop("token missing", call. = FALSE)

  # Find .Renviron path
  path <- get_renv_path(type = c("user", "project"),
                        envvar = "R_ENVIRON_USER")

  action <- match.arg(action,
                      c("create", "overwrite", "delete"),
                      several.ok = FALSE)

  envir <- ""
  if(file.exists(path)) envir <- readLines(path)

  token_exists <- grep(paste0("^", token_name, "="), envir)
  envir_new <- switch(action,
                      "delete" = {
                        if(length(token_exists) != 0){
                          message("Deleting token name '", token_name,"'")
                          envir <- envir[-token_exists]
                          envir
                        }else{
                          stop("Token name '", token_name,
                               "' does not exists, nothing to delete.")
                        }
                      },
                      "overwrite" = {
                        if(length(token_exists) != 0){
                          message("Token name '", token_name,
                                  "' already exists, forcing an overwrite.\n",
                                  "R session must be rephreshed for new token to take effect.")
                          envir[token_exists] <- paste(token_name, token, sep="=")
                          envir
                        }else{
                          message("Token name '", token_name,
                                  "' does not already exists, adding new token.\n",
                                  "R session must be rephreshed for new token to take effect.")
                          envir[length(envir)+1] <- paste(token_name, token, sep="=")
                          envir
                        }
                      },
                      "create" = {
                        if(length(token_exists) != 0){
                          stop("Token name '", token_name,
                               "' already exists. If you want to overwrite it, use action = 'overwrite'",
                               call. = FALSE)
                        }else{
                          envir[length(envir)+1] <- paste(token_name, token, sep="=")
                          message("Token name '", token_name,
                                  "' added.")
                          envir
                        }
                      })

  # Write updated environment variables
  writeLines(envir_new, path)

  # Make sure the file is only accessible to the user
  Sys.chmod(path, mode = "0700")
}

#' Open .Renviron file for editing
#'
#' Will open the .Renviron file for editing.
#' In this file you set environment variables
#' that might be required for using certain
#' command line arguments through functions.
#' In this package the .Renviron stores the
#' API access tokens necessary to communicate
#' securely with nettskjema.
#'
#' @export
#' @return No return value. Opens Renviron file.
#' @importFrom usethis edit_r_environ
#' @examples
#' \dontrun{
#' nettskjema_renviron_edit()
#' }
nettskjema_renviron_edit <- function(){
  edit_r_environ()
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

#' @noRd
#' @template token_name
api_auth <- function(token_name = "NETTSKJEMA_API_TOKEN"){
  paste("Bearer", Sys.getenv(token_name))
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

