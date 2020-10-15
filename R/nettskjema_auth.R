
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
  if(Sys.getenv(token_name) == "") stop(paste0("Token with name '", token_name, "' does not exist."),
                                        call. =FALSE)

  resp <- nettskjema_api("users/admin/tokens/expire-date",
                         token_name = token_name)

  api_catch_error(resp)

  dt <- as.Date(unlist(content(resp)))

  message(paste0("Token with name '", token_name, "' expires in ",
                 as.numeric(dt - Sys.Date()),
                 " days."
  ))

  dt
}


#' Create Nettskjema API user
#'
#' Opens OS browser to create API user.
#'
#' @importFrom utils browseURL
#' @export
nettskjema_user_create <- function(){
  browseURL("https://nettskjema.uio.no/user/api/index.html")
}

#' Write token to .Renviron
#'
#' This function will write a token
#' to the .Renviron file with the
#' name you provide.
#'
#' @param token character. Token generated in the UiO portal \code{api_user_create}
#' @param action character. One of three actions: 'create', 'overwrite' or 'delete'.
#' Defaults to 'create'.
#' @inheritParams nettskjema_get_forms
#'
#' @export
nettskjema_token2renviron <- function(token,
                                      token_name = "NETTSKJEMA_API_TOKEN",
                                      action = c("create", "overwrite", "delete")){
  if(missing(token)){
    stop("token missing", call. = FALSE)
  }

  # Find .Renviron path
  path <- get_renv_path(type = c("user", "project"),
                        envvar = "R_ENVIRON_USER")

  action <- match.arg(action,
                      c("create", "overwrite", "delete"),
                      several.ok = FALSE)

  envir <- if(file.exists(path)){
    readLines(path)
  }else{
    ""
  }

  token_exists <- grep(paste0("^", token_name, "="), envir)
  envir_new <- switch(action,
                      "delete" = {
                        if(length(token_exists) != 0){
                          message(paste0("Deleting token name '", token_name,"'"))
                          envir <- envir[-token_exists]
                          envir
                        }else{
                          stop(paste0("Token name '", token_name,
                                      "' does not exists, nothing to delete."))
                        }
                      },
                      "overwrite" = {
                        if(length(token_exists) != 0){
                          message(paste0("Token name '", token_name,
                                         "' already exists, forcing an overwrite."))
                          envir[token_exists] <- paste(token_name, token, sep="=")
                          envir
                        }else{
                          message(paste0("Token name '", token_name,
                                         "' does not already exists, adding new token"))
                          envir[length(envir)+1] <- paste(token_name, token, sep="=")
                          envir
                        }
                      },
                      "create" = {
                        if(length(token_exists) != 0){
                          stop(paste0("Token name '", token_name,
                                      "' already exists. If you want to overwrite it, use action = 'overwrite'"),
                               call. = FALSE)
                        }else{
                          envir[length(envir)+1] <- paste(token_name, token, sep="=")
                          message(paste0("Token name '", token_name,
                                         "' added."))
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
#' @importFrom usethis edit_r_environ
nettskjema_renviron_edit <- function(){
  edit_r_environ()
}

#' nettskjema api connection
#'
#' @param path path connection
#' @param token_name token name for lookup
#' @param ... arguments passed to GET
#'
#' @return an httr reponse
#' @importFrom httr GET add_headers
nettskjema_api <- function(path, token_name, ...) {
  url <- paste0("http://nettskjema.no/api/v2/", path)
  GET(url,
      ...,
      add_headers(Authorization = api_auth(token_name))
  )
}

api_auth <- function(token_name = "NETTSKJEMA_API_TOKEN"){
  paste("Bearer", Sys.getenv(token_name))
}


