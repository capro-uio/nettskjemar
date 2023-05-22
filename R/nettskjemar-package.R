#' nettskjemar: Interact with Nettskjema survey tool from University of Oslo
#'
#' @name nettskjemar
#' @docType package
#' @keywords internal
"_PACKAGE"


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(
    "question_codebook",
    "cb",
    "question",
    "question_id",
    "answer",
    "columns",
    "string",
    "value",
    "form_id",
    "element_no",
    "submission_id"))
}