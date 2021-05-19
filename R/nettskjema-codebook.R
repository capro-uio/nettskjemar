#' Constructor for nettskjema codebook
#'
#' Function works on meta-data downloaded from a form.
#' It will return a data.frame in long format with
#' meta-data information from a specific nettskjema form.
#'
#' @param meta_data form meta-data from \code{\link{nettskjema_get_meta}}
#' @template form_id
#' @return long format tibble with meta-data for a specific nettskjema
#' @export
#'
#' @examples
#' \dontrun{
#' meta_110000 <- nettskjema_get_meta(110000)
#' codebook(meta_110000)
#' }
#' @importFrom dplyr bind_rows relocate
codebook <- function(meta_data, form_id){
  els <- meta_data$elements$details

  idx <- which(unlist(lapply(els, is.data.frame)))

  codes <- lapply(idx,
                  function(x) els[[x]])
  names(codes) <- meta_data$elements$order[idx]

  dt <- bind_rows(codes, .id = "element_no")
  dt$form_id <- form_id
  dt <- relocate(dt, form_id, element_no)

  structure(
    dt, class = c("nettskjema_codebook", class(dt))
  )
}

#' Get codebook for a form
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#'
#' @template form_id
#' @template token_name
#' @template as_is
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return list of class nettskjema_meta_data
#' @export
#' @importFrom httr content
#' @examples
#' \dontrun{
#' codebook_110000 <- nettskjema_get_codebook(110000)
#' }
nettskjema_get_codebook <- function(form_id,
                                    as_is = FALSE,
                                    token_name = "NETTSKJEMA_API_TOKEN",
                                    ...){

  if(as_is){
    cb <- get_raw_codebook(form_id = form_id,
                           token_name = token_name,
                           ...)
    return(cb)
  }

  # Get codebook based on the meta-data elements
  meta <- nettskjema_get_meta(form_id = form_id,
                              as_is = FALSE,
                              token_name = token_name,
                              ...)

  codebook(meta, form_id)
}


#' Get the raw codebook
#'
#' The raw codebook is a list
#' of answers and questions with
#' unique ids for both, and the text associated
#' with them. This information can be retrieved
#' from the raw nettskjema data. This can be useful
#' if the nettskjema submission data has been returned
#' with \code{as_is = TRUE}.
#' @template form_id
#' @template token_name
#' @param ... other arguments to \code{\link[httr]{GET}}
#' @return long format tibble with meta-data for a specific nettskjema
#' @examples
#' \dontrun{
#' form_id <- 100000
#' get_raw_codebook(form_id)
#'
#' Or if the token is saved in a non-standard name
#' get_raw_codebook(form_id, token_name = "MY_NETTSKJEMA_TOKEN_NAME")
#' }
get_raw_codebook <- function(form_id, token_name = "NETTSKJEMA_API_TOKEN", ...){

  path <- file.path("forms", form_id, "codebook")
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
    ), class = c("nettskjema_codebook_raw", "list")
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



