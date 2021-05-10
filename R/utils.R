
#' @importFrom rvest html_text
#' @importFrom xml2 read_html
strip_html <- function(s) {
  if(!is.na(s))
    s <- gsub("\\\n|\\\t", "", html_text(read_html(s)))

  s
}

max_selected <- function(x){
  t <- x$maxSelectedAnswerOptions
  ifelse(t == 0, NaN, t)
}

check_element <- function(x){
  if(length(x) == 1)
    return(x)

  NULL
}

#' @importFrom stats setNames
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

get_renv_path <- function(type = c("user", "project"),
                          envvar = "R_ENVIRON_USER"){
  envvar <- Sys.getenv(envvar)

  if(envvar != "") return(envvar)

  type <- match.arg(type, c("user", "project"))

  type <- switch(type,
                 "user" = Sys.getenv("HOME"),
                 "project" = here::here()
  )

  file.path(file.path(type, ".Renviron"))
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("question_codebook","cb", "question", "answer", "columns",
                           "string", "value",
                           "form_id", "element_no", "submission_id"))
}
