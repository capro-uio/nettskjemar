valid_form_inf <- function(){
  c("title", "language",
    "created", "modified", "opened",
    "respondents", "contact","codebook",
    "personal_data", "sensitive_data",
    "editors", "elements")
}

strip_html <- function(string) {
  gsub("<[^>]*>", "", string)
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


#' Check if form has codebook
#'
#' Codebook is by default turned off
#' in Nettskjema, but best practices in handling
#' data is to have it on. This function
#' checks if the codebook of a form has been activated
#' or not
#'
#' @template form_id
#' @template token_name
#'
#' @return logical is codebook is turned on
#' @export
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#' has_codebook(110000)
#' }
has_codebook <- function(form_id, token_name = "NETTSKJEMA_API_TOKEN"){
  nettskjema_get_meta(form_id, token_name = token_name)$codebook
}

rm_ext <- function(file){
  ex <- tools::file_ext(file)
  gsub(ex, "", file)
}

# will list the submission ids associated with a form
list_submissions <- function(path, opts, token_name, ...) {
  path_inc <- paste0(path, opts, "fields=submissionId")
  resp <- nettskjema_api(path_inc, token_name = token_name, ...)
  api_catch_error(resp)
  unlist(content(resp))
}

# make options for getting form data
make_opts <- function(from_date = "", from_submission = ""){
  if(from_date != "" ){
    from_date <- sprintf("fromDate=%s", from_date)
  }

  if(from_submission != "" ){
    from_submission <- sprintf("fromSubmissionId=%s", from_submission)
  }

  sprintf("?%s&%s", from_date, from_submission)
}

# get global options
get_option = function(x, default = NULL){
  getOption("test") %||% default
}

# assign b if a is nothing
`%||%` <- function(a, b){
  if(length(a) == 0) return(b)
  if(is.na(a) | is.null(a) | a == "") return(b)
  a
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("question_codebook","cb", "question", "answer", "columns",
                           "string", "value",
                           "form_id", "element_no", "submission_id"))
}
