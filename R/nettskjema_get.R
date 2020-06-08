#' Get data from a form
#'
#' This function connects to a specific form and fetches all answers.
#' The reponses are placed in a data.frame for easy use.
#' For large data sets, toggle the \code{incremental} option,
#' which while slow, will be better able to retrieve all responses.
#' For retrieving responses only from a specific date on, or all responses
#' after a certain submission, use the \code{from_date} and \code{from_submission} arguments.
#' Forms that are anonymous and do not collect personal information do not record date,
#' and as such the \code{from_date} will not work on those and an error will be thrown.
#'
#' @param form_id integer. Number of the form to retrieve
#' @param use_codebook logical. Use codebook for answers, or
#' retrieve actualy text options.
#' data from
#' @param token_name character. Name to give the token, defaults to 'NETTSKJEMA_API_TOKEN'
# #' @param incremental numeric. Incremental size of number of responses to fetch incrementally.
# #' Necessary for successful fetching of large number of responses. Default (NULL) fetches all at once.
#' @param incremental logical. False fetches all at once, TRUE fetches each submission individually. Slower but more stable for larger datasets.
#' @param from_date date. From which date on should data be fetched for
#' @param from_submission integer. From which SubmissionId should data be collected from.
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return tibble data.frame
#' @export
nettskjema_get_data <- function(form_id,
                                use_codebook = TRUE,
                                token_name = "NETTSKJEMA_API_TOKEN",
                                from_date = "",
                                from_submission = "",
                                incremental = FALSE,
                                ...){

  path = file.path("forms", form_id, "submissions")

  if(!incremental){

    if(from_date != "" ){
      from_date <- paste0("fromDate=", from_date)
    }

    if(from_submission != "" ){
      from_submission <- paste0("fromSubmissionId=", from_submission)
    }

    opts <- paste0("?", from_date, from_submission)

    resp <- nettskjema_api(paste0(path, opts), token_name = token_name, ...)

    api_catch_error(resp)

    cont <- httr::content(resp)

  }else{
    # get all submissionIds first, to create increments
    path_inc <- paste0(path, "?fields=submissionId")
    resp_inc <- nettskjema_api(path_inc, token_name = token_name, ...)

    api_catch_error(resp_inc)

    submissionIds <- unlist(httr::content(resp_inc))

    if(from_submission != "") submissionIds[submissionIds > from_submission]

    submissionIds <- file.path("submissions", submissionIds)

    resp <- pbapply::pblapply(submissionIds,
                              function(x) nettskjema_api(x,
                                                         token_name = token_name, ...)
    )

    j <- lapply(resp, api_catch_error)

    cont <- lapply(resp, httr::content)
  }

  # Add form_id to the outputed data
  dt <- dplyr::mutate(clean_form_submissions(cont),
                      form_id = form_id)
  dplyr::select(dt, form_id, dplyr::everything())
}

#' Get all forms you have access to
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#' TODO: make this work
#'
#' @inheritParams nettskjema_get_data
#'
#' @return list
nettskjema_get_forms <- function(token_name = "NETTSKJEMA_API_TOKEN", ...){

  resp <- nettskjema_api("forms/", token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  httr::content(resp)

}

#' Get metadata for a form
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#'
#' @inheritParams nettskjema_get_forms
#' @inheritParams nettskjema_get_data
#' @param information which meta data elements to extract
#' @param ...  arguments to httr::GET
#'
#' @return list
#' @export
nettskjema_get_meta <- function(form_id,
                                information = c("title", "language",
                                                "created", "modified", "opened",
                                                "respondents", "contact","codebook",
                                                "personal_data", "sensitive_data",
                                                "editors", "elements"),
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  path = paste0("forms/", form_id)
  resp <- nettskjema_api(path, token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  information <- match.arg(information,
                           c("title", "language",
                             "created", "modified", "opened",
                             "respondents", "contact","codebook",
                             "personal_data", "sensitive_data",
                             "editors", "elements"),
                           several.ok = TRUE)

  fields_idx <- meta_fields() %in% information
  fields <- names(meta_fields())[fields_idx]

  cont <- httr::content(resp)

  dt <- lapply(fields, function(x) cont[[x]])
  names(dt) <- meta_fields()[fields_idx]
  meta_classes(dt)
}