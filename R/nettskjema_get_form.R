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
#'@details Checkbox types
#' \itemize{
#'  \item{"string"}{ - returns a delimited character value per submission, where \code{checkbox_delim}
#'  denotes the options that the respondent has chosen}
#'  \item{"list"}{ - returns a list column, where each submission has a character vector with all the
#'  chosen options as separate elements in the list}
#'  \item{"columns"}{ - returns checkbox answers as separate binarized columns (column names are appended with
#'  response names), where 1 means the option was selected and 0 it was not.}
#' }
#'
#' @template form_id
#' @template use_codebook
#' @template information
#' @template token_name
#' @template as_is
#' @param checkbox_type string of either "string" (default), "list" or "columns" for how to handle checkbox answers
#' @param checkbox_delim delimiter string if \code{checkbox:type} is "string". Ignored else.
#' @param incremental logical. False fetches all at once, TRUE fetches each submission individually. Slower but more stable for larger datasets.
#' @param from_date date. From which date on should data be fetched for
#' @param from_submission integer. From which SubmissionId should data be collected from.
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return tibble data.frame
#' @export
#' @importFrom dplyr mutate relocate
#' @examples
#' \dontrun{
#'
#' # Retrieve all data
#' data_110000 <- nettskjema_get_data(110000)
#
#' # Retrieve all data, and add answer order and option text
#' data_110000 <- nettskjema_get_data(110000, information = list(dummy = "order", text = "option"))
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
                                information = NULL,
                                use_codebook = has_codebook(form_id),
                                checkbox_type = c("string", "list", "columns"),
                                checkbox_delim = ";",
                                as_is = FALSE,
                                from_date = "",
                                from_submission = "",
                                incremental = FALSE,
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  if(!use_codebook & !is.null(information)){
    warning("Cannot combine `use_codebook` and `information`, setting `information = NULL`",
            call. = FALSE)
    information <- NULL
  }

  path = file.path("forms", form_id, "submissions")

  if(from_date != "" ){
    from_date <- sprintf("fromDate=%s", from_date)
  }

  if(from_submission != "" ){
    from_submission <- sprintf("fromSubmissionId=%s", from_submission)
  }

  opts <- paste0("?", from_date, from_submission)

  # get all submissionIds first, to create increments
  path_inc <- paste0(path, opts, "fields=submissionId")
  resp_inc <- nettskjema_api(path_inc, token_name = token_name, ...)

  api_catch_error(resp_inc)

  submissionIds <- unlist(content(resp_inc))

  if(from_submission != "") submissionIds[submissionIds > from_submission]
  cat(sprintf("Form %s has %s responses to download.\n",form_id, length(submissionIds)))

  cont <- grab_data(incremental, submissionIds,
                    token_name, path, opts, ...)

  # If asked to return as_is, then return the
  # raw json content
  if(as_is) return(cont)

  cb <- nettskjema_get_codebook(form_id = form_id,
                                as_is = TRUE,
                                token_name = token_name,
                                ...)

  m <- nettskjema_get_meta(form_id)
  if(!m$codebook){
    warning("No codebook defined for this form. Setting 'use_codebook' to FALSE.",
            call. = FALSE)
    cb <- codebook(m, form_id)
    use_codebook = FALSE
  }

  dt <- clean_form_submissions(cont,
                               cb = cb,
                               use_codebook = use_codebook,
                               checkbox_type = checkbox_type,
                               checkbox_delim = checkbox_delim)

  # Add form_id to the outputted data
  dt <- mutate(dt, form_id = form_id)

  if(!is.null(information)){
    cb <- nettskjema_get_codebook(form_id = form_id, token_name = token_name)
    dt <- nettskjema_get_extra(data = dt,
                               codebook = cb,
                               information = information,
                               use_codebook = use_codebook)
  }

  dt <- dt[,order(colnames(dt))]
  relocate(dt, form_id, submission_id)
}

# #' Get all forms you have access to
# #'
# #' With the given API token, will retrieve
# #' a list of all the forms you have access to
# #' TODO: Wait for IT to make this possible
# #'
# #' @template token_name
# #' @template as_is
# #' @param ... arguments passed to \code{\link[httr]{GET}}
# #'
# #' @return tibble
# #' @importFrom httr content
# #' @export
# nettskjema_get_forms <- function(token_name = "NETTSKJEMA_API_TOKEN",
#                                  as_is = FALSE, ...){
#
#   resp <- nettskjema_api("forms/", token_name = token_name, ...)
#
#   api_catch_error(resp)
#   api_catch_empty(resp)
#
#   cont <- content(resp)
#
#   if(as_is) return(cont)
#
# }


#' Add additional answer data
#'
#' The answers in the nettskjema forms
#' have some extra information attached
#' that is not returned by default.
#' This is for instance information on
#' the order of the answer options, the true
#' text, whether the answer is correct or
#' if it is preselected. This function makes
#' it possible to retrieve and add this
#' information to the nettskjema data, using
#' the codebook.
#'
#' @template data
#' @param information character vector of information to add.
#' One or more of "order", "option", "correct" "preselected".
#' @param codebook codebook object retrieved by \code{\link{nettskjema_get_codebook}}
#' @template use_codebook
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#'
#' @return tibble with added columns
#' @export
#' @examples
#' \dontrun{
#' form_id <- 100000
#'
#' my_data <- nettskjema_get_data(form_id)
#' cb <-  nettskjema_get_codebook(form_id)
#'
#' my_data <- nettskjema_get_extra(my_data, cb, "order")
#'
#' }
nettskjema_get_extra <- function(data,
                                 codebook,
                                 information,
                                 use_codebook = TRUE,
                                 ...){

  if(missing(information)){
    stop("No argument passed to `information`, no extra data to add",
            call. = FALSE)
  }

  information <- validate_information(information)

  # reduce codebook to only those with answer options
  questions <- NA
  if(!inherits(codebook, "data.frame")){
    type <- ifelse(use_codebook, "question_codebook", "question")
    type_answ <- ifelse(use_codebook, "answer_codebook", "answer_option")

    cb <- codebook[!is.na(codebook[,type_answ]),]

    # Those without order are check-boxes
    cb <- cb[!is.na(cb[,"question_order"]),]

    # get unique questions
    questions <- unname(unlist(unique(cb[,type])))
  }


  # If they have set up questions that are not represented in the actual
  # question part of the form, this process will fail to merge properly.
  if(any(is.na(questions)))
    stop("The codebook is not set up correctly, or some questions do not have text. ",
         "Adding extra information from the codebook is not possible in this situation. ",
         "Try filling out the codebook, before downloading the data once more.\n",
         call. = FALSE)

  get_extra_data(questions, col,
                 type, type_answ,
                 data, information, cb)

}

