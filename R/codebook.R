# Grab codebook for a form
#
# Function works on meta-data downloaded from a form.
# It will return a data.frame in long format
#
# @param meta_data form meta-data from \code{\link{nettskjema_get_meta}}
#
# @return long format tibble
# @export
#
# @examples
# \dontrun{
# meta_110000 <- nettskjema_get_meta(110000)
# codebook(meta_110000)
# }
#' @importFrom dplyr bind_rows relocate
codebook <- function(meta_data){
  els <- meta_data$elements$details

  idx <- which(unlist(lapply(els, is.data.frame)))

  codes <- lapply(idx,
                  function(x) els[[x]])
  names(codes) <- meta_data$elements$order[idx]

  dt <- bind_rows(codes, .id = "element_no")
  relocate(dt, element_no)
}

# Get the raw codebook
#
# The raw codebook is a list
# of answers and questions with
# unique ids for both, and the text associated
# with them. This information can be retrieved
# from the raw nettskjema data. This can be useful
# if the nettskjema submission data has been returned
# with \code{as_is = TRUE}.
get_raw_codebook <- function(form_id, token_name = "NETTSKJEMA_API_TOKEN", ...){

  path = file.path("forms", form_id, "codebook")
  resp <- nettskjema_api(path, token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  dt <- content(resp)

  # remove NULLs
  idx <- which(!sapply(dt$externalQuestionIds, is.null))
  dt$externalQuestionIds <- lapply(idx, function(x) dt$externalQuestionIds[[x]])

  # remove NULLs
  idx <- which(!sapply(dt$externalAnswerOptionIds, is.null))
  dt$externalAnswerOptionIds <- lapply(idx, function(x) dt$externalAnswerOptionIds[[x]])

  structure(
    list(
      form_id = form_id,
      questions = dplyr::tibble(
        id = names(dt$externalQuestionIds),
        text = unlist(unname(dt$externalQuestionIds))
      ),
      answers = dplyr::tibble(
        id = names(dt$externalAnswerOptionIds),
        text = unlist(unname(dt$externalAnswerOptionIds))
      )
    ), class = "nettskjema_codebook_raw"
  )
}

#' @export
format.nettskjema_codebook_raw <- function(x, ...){

  c(
    sprintf("# Nettskjema raw codebook for form %s", x$form_id),
    "",
    sprintf("no. questions: %s", nrow(x$questions)),
    sprintf("no. answers: %s", nrow(x$answers))
  )
}

#' @export
print.nettskjema_codebook_raw <- function(x, ...){
  cat(format(x), sep="\n")
  invisible(x)
}



