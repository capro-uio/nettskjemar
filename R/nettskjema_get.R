#' Get data from a form
#'
#' This function connects to a specific form and fetches all answers.
#' The responses are placed in a data.frame for easy use.
#' For large data sets, toggle the \code{incremental} option,
#' which while slow, will be better able to retrieve all responses.
#' For retrieving responses only from a specific date on, or all responses
#' after a certain submission, use the \code{from_date} and \code{from_submission} arguments.
#' Forms that are anonymous and do not collect personal information do not record date,
#' and as such the \code{from_date} will not work on those and an error will be thrown.
#'
#' @template form_id
#' @param additional_data character vector of additional answer information
#' to add (One or more of "order", "option", "correct" "preselected"). If NLLL (default) nothing is added.
#' @template token_name
#' @param incremental logical. False fetches all at once, TRUE fetches each submission individually. Slower but more stable for larger datasets.
#' @param from_date date. From which date on should data be fetched for
#' @param from_submission integer. From which SubmissionId should data be collected from.
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return tibble data.frame
#' @export
#' @importFrom dplyr mutate select everything
#' @importFrom httr content
#' @importFrom pbapply pblapply
#' @examples
#' \dontrun{
#'
#' # Retrieve all data
#' data_110000 <- nettskjema_get_data(110000)
#
#' # Retrieve all data, and add answer order and option text
#' data_110000 <- nettskjema_get_data(110000, add)
#'
#' # Retrieve all data after a certain date
#' data_110000 <- nettskjema_get_data(110000, from_date = "2019-01-01")
#'
#' # Retrieve all submissions after a specific submission id
#' data_110000 <- nettskjema_get_data(110000, from_submission = "12000000")
#'
#' # For large data
#' data_110000 <- nettskjema_get_data(110000, incremental = TRUE)
#'
#' }
nettskjema_get_data <- function(form_id,
                                additional_data = NULL,
                                from_date = "",
                                from_submission = "",
                                incremental = FALSE,
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  path = file.path("forms", form_id, "submissions")

  if(from_date != "" ){
    from_date <- paste0("fromDate=", from_date)
  }

  if(from_submission != "" ){
    from_submission <- paste0("fromSubmissionId=", from_submission)
  }

  opts <- paste0("?", from_date, from_submission)

  # get all submissionIds first, to create increments
  path_inc <- paste0(path, opts, "fields=submissionId")
  resp_inc <- nettskjema_api(path_inc, token_name = token_name, ...)

  api_catch_error(resp_inc)

  submissionIds <- unlist(content(resp_inc))

  if(from_submission != "") submissionIds[submissionIds > from_submission]
  cat(sprintf("There are %s responses to download.\n", length(submissionIds)))

  if(incremental | length(submissionIds) > 1000){

    if(length(submissionIds) > 1000)
      cat("Number of responses to download exceeds 1000, switching to incremental download.\n")

    submissionIds <- file.path("submissions", submissionIds)

    resp <- pblapply(
      submissionIds,
      function(x) nettskjema_api(x,
                                 token_name = token_name, ...)
    )

    j <- lapply(resp, api_catch_error)

    cont <- lapply(resp, content)
  }else{

    resp <- nettskjema_api(paste0(path, opts),
                           token_name = token_name, ...)

    api_catch_error(resp)

    cont <- content(resp)

  }

  # Add form_id to the outputted data
  dt <- mutate(clean_form_submissions(cont),
               form_id = form_id)
  dt <- select(dt, form_id, everything())

  if(!is.null(additional_data)){
    cb <- nettskjema_get_codebook(form_id, token_name)
    dt <- add_answer_data(dt, cb, additional_data)
  }
  dt
}

#' Get all forms you have access to
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#' TODO: make this work
#'
#' @template token_name
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return tibble
#' @importFrom httr content
nettskjema_get_forms <- function(token_name = "NETTSKJEMA_API_TOKEN", ...){

  resp <- nettskjema_api("forms/", token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  content(resp)

}

#' Get metadata for a form
#'
#' With the given API token, will retrieve
#' the meta-data connected to a form.
#'
#' @template form_id
#' @template token_name
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return list of class nettskjema_meta_data
#' @export
#' @importFrom httr content
#' @examples
#' \dontrun{
#' meta_110000 <- nettskjema_get_meta(110000)
#'
#' }
nettskjema_get_meta <- function(form_id,
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  path = file.path("forms", form_id)
  resp <- nettskjema_api(path, token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  information <- c("title", "language",
                   "created", "modified", "opened",
                   "respondents", "contact","codebook",
                   "personal_data", "sensitive_data",
                   "editors", "elements")

  fields_idx <- meta_fields() %in% information
  fields <- names(meta_fields())[fields_idx]

  cont <- content(resp)

  dt <- lapply(fields, function(x) cont[[x]])
  names(dt) <- meta_fields()[fields_idx]

  dt$form_id = form_id
  meta_classes(dt)
}


#' Get metadata for a form
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#'
#' @template form_id
#' @template token_name
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
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  # Get codebook based on the meta-data elements
  meta <- nettskjema_get_meta(form_id, token_name, ...)
  codebook(meta)
}