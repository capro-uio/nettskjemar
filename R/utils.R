
#' @importFrom rvest html_text
#' @importFrom xml2 read_html
strip_html <- function(s) {
  html_text(read_html(s))
}

max_selected <- function(x){
  t <- x$maxSelectedAnswerOptions
  ifelse(t == 0, NaN, t)
}

is.response <- function(x) class(x) == "response"

is.app_json <- function(x){
  if(!is.response(x)) return(FALSE)
  http_type(x) == "application/json"
}

#' @importFrom httr http_type content http_error status_code
#' @importFrom jsonlite fromJSON
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
api_catch_empty <- function(resp){
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if (resp$status_code == 500) {
    stop(
      sprintf(
        "Nettskjema API request with \n %s",
        http_status(resp)$message),
      call. = FALSE
    )
  }
}

check_element <- function(x){
  if(length(x) == 1)
    return(x)

  NULL
}

validate_information <- function(information) {

  inf_nms <- if(is.null(names(information))){
    information
  }else{
    names(information)
  }

  setNames(match.arg(unlist(information),
                     info(),
                     several.ok = TRUE),
           inf_nms)
}

info <- function(){
  c("order", "option",
    "correct", "preselected")
}


rn_cols <- function(x, from, to){
 gsub(paste0(from, "$"), to, x)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("question_codebook","cb",
                           "form_id", "element_no", "submission_id"))
}
