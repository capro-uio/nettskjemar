#' @noRd
is.response <- function(x) inherits(x, "response")

#' @noRd
is.app_json <- function(x){
  if(!is.response(x)) return(FALSE)
  http_type(x) == "application/json"
}

#' @importFrom httr http_type content http_error status_code
#' @importFrom jsonlite fromJSON
#' @noRd
api_catch_error <- function(resp){
  if (!is.app_json(resp)) {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text"),
                     simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "Nettskjema API request failed with error\n %s : %s\n",
        status_code(resp), parsed$message),
      call. = FALSE
    )
  }
}


#' @importFrom httr http_type content http_status
#' @importFrom jsonlite fromJSON
#' @noRd
api_catch_empty <- function(resp){
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text"),
                     simplifyVector = FALSE)

  if (resp$status_code == 500) {
    stop(
      sprintf(
        "Nettskjema API request with \n %s",
        http_status(resp)$message),
      call. = FALSE
    )
  }
}
