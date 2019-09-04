
#' Check nettskjema API token expiry date
#'
#' Given an API token, will retrieve the expiry
#' date of the said token.
#'
#' @param token_name Given name of the token
#'
#' @return character date string
#' @export
nettskjema_token_expiry <- function(token_name = "NETTSKJEMA_API_TOKEN"){

  # Check that token exists in env
  if(Sys.getenv(token_name) == "") stop(paste0("Token with name '", token_name, "' does not exist."),
                                        call. =FALSE)

  resp <- nettskjema_api("users/admin/tokens/expire-date",
                      token_name = token_name)

  api_catch_error(resp)

  dt <- as.Date(unlist(httr::content(resp)))

  message(paste0("Token with name '", token_name, "' expires in ",
                as.numeric(dt - Sys.Date()),
                " days."
  ))

  dt
}


#' TODO: Add possibility to supply username and description through function
#' @export
nettskjema_user_create <- function(){
  utils::browseURL("https://nettskjema.uio.no/user/api/index.html")
}

#' Write token to .Renviron
#'
#' This function will write a token
#' to the .Renviron file with the
#' name you provide.
#'
#' @param token character. Token generated in the UiO portal \code{api_user_create}
#' @param token_name character. Name to give the token, defaults to 'NETTSKJEMA_API_TOKEN'
#' @param overwrite logical. Overwrite token if token by that name already exists.
#' Defaults to FALSE
#'
#' @export
nettskjema_token2renviron <- function(token,
                               token_name = "NETTSKJEMA_API_TOKEN",
                               overwrite = FALSE){
  path <- usethis:::scoped_path_r(c("user", "project"),
                                  ".Renviron",
                                  envvar = "R_ENVIRON_USER")
  envir <- readLines(path)

  token_exists <- grep(paste0("^", token_name, "="), envir)
  if(length(token_exists) != 0){
    if(force){
      message(paste0("Token name '", token_name,
                     "' already exists, forcing an overwrite."))
      envir[token_exists] <- paste(token_name, token, sep="=")
    }else{
      stop(paste0("Token name '", token_name,
                  "' already exists. Pick another name, or force an overwrite."))
    }
  }else{
    envir[length(envir)+1] <- paste(token_name, token, sep="=")
  }

  # Write updated environment variables
  writeLines(envir, path)

  # Make sure the file is only accessible to the user
  Sys.chmod(path, mode = "0400")
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
nettskjema_renviron_edit <- function(){
  usethis::edit_r_environ()
}

nettskjema_api <- function(path, token_name) {
  url <- paste0("http://nettskjema.no/api/v2/", path)
  httr::GET(url,
      httr::add_headers(Authorization = api_auth(token_name))
  )
}

api_auth <- function(token_name = "NETTSKJEMA_API_TOKEN"){
  paste("Bearer", Sys.getenv(token_name))
}

api_catch_error <- function(resp){
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      paste(
        "Nettskjema API request failed with error",
        paste(httr::status_code(resp), parsed$message, sep=": "),
        sep="\n"
      ),
      call. = FALSE
    )
  }
}


api_catch_empty <- function(resp){
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (resp$status_code == 500) {
    stop(
      paste(
        "Nettskjema API request with",
        httr::http_status(resp)$message,
        sep="\n"
      ),
      call. = FALSE
    )
  }
}
