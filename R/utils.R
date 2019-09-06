
strip_html <- function(s) {
  rvest::html_text(xml2::read_html(s))
}

max_selected <- function(x){
  t <- x$maxSelectedAnswerOptions
  ifelse(t == 0, NaN, t)
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
