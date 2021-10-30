

#' Retrieve attachments associated with a Nettskjema
#'
#' Some Nettskjema have fields for attachments. These
#' can be downloaded through this function. There
#' are two naming convention that may be used for the
#' saved files' names. See output of
#' \code{\link{nettskjema_list_attachments}} for idea
#' of the names used.
#'
#' @details 'filenames types
#' \itemize{
#'  \item{"original"}{ - uses file names are they were
#'     uploaded to Nettskjema}
#'  \item{"standardised"}{ - creates file names based on
#'     the submission id and a counter number to uniquely
#'     create file names for each submission (in case there
#'     are more than one attachment)}
#'  }
#'
#' @template form_id
#' @param filenames string of either 'standardised' (default) or
#'     'original' indicating which file names to use.
#' @param output_dir directory to output the files to
#' @template token_name
#' @template from_date
#' @template from_submission
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return invisible named logical if the file was saved successfully
#' @export
#'
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' nettskjema_get_form_attachments(form_id)
#'
#' # save files to specific folder
#' nettskjema_get_form_attachments(form_id, output_dir = "~/Desktop")
#'
#' # save using original file names
#' nettskjema_get_form_attachments(form_id, filenames = "original")
#'
#' }
nettskjema_get_form_attachments <- function(form_id,
                                            filenames = c("standardised", "original"),
                                            output_dir = ".",
                                            token_name = "NETTSKJEMA_API_TOKEN",
                                            from_date = "",
                                            from_submission = "",
                                            ...){
  filenames <- match.arg(filenames, c("standardised", "original"))
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  path = file.path("forms", form_id, "submissions")
  submission_ids <- list_submissions(path,
                                     make_opts(from_date, from_submission),
                                     token_name, ...)
  ats <- nettskjema_list_attachments(submission_ids,
                                     token_name, ...)
  invisible(
    mapply(nettskjema_save_attachment,
           path = ats$path,
           output = file.path(output_dir, unlist(ats[, filenames])),
           MoreArgs = list(
             token_name , ...
           ))
  )
}

#' List Nettskjema attachments
#'
#' Each unique submission ID in Nettskjema
#' may have some attachments. These can be listed
#' using this function, which will provide the
#' information on where in the Nettskjema API the
#' files are located, their original file names,
#' and a suggested standardised file names for
#' tidier data output.
#'
#' @template submission_id
#' @template token_name
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return a tibble with information on attachments available.
#' @export
#' @importFrom dplyr as_tibble
#' @examples
#' \dontrun{
#' submission_id <- c(22222, 1232, 21555)
#' nettskjema_list_attachments(submission_id)
#'
#' }
nettskjema_list_attachments <- function(submission_id,
                                        token_name = "NETTSKJEMA_API_TOKEN",
                                        ...){
  ats <- lapply(submission_id, fetch_attachment, token_name, ...)
  ats <- do.call(rbind, ats)
  as_tibble(ats)
}

#' Save Nettskjema attachments to file
#'
#' The Nettskjema forms has an option
#' to upload attachments with forms.
#' These can be retrieved by this function.
#' Recommended workflow is to first call
#' \code{\link{nettskjema_list_attachments}},
#' and use the output of this to pass along
#' to the function. This function is called
#' by \code{\link{nettskjema_get_form_attachments}},
#' but you can use it to define your own output
#' file names.
#'
#' @param path Nettskjema API path where the attachment is
#' @param output output file name
#' @template token_name
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @importFrom base64enc base64decode
#' @importFrom httr content
#' @return nothing. saves attachments to file.
#' @export
#'
#' @examples
#' \dontrun{
#' submission_id <- c(22222, 1232, 21555)
#' attach_dt <- nettskjema_list_attachments(submission_id)
#' nettskjema_save_attachment(path = attach_dt$path, output = attach_dt$standardised)
#' }
nettskjema_save_attachment <- function(path, output, token_name , ...){
  resp <- nettskjema_api(path, token_name , ...)
  api_catch_error(resp)
  attachment <- content(resp)$content
  out <- file(output, "wb")
  on.exit(close(out))
  message("Saving attachment to ", output)
  base64decode(attachment, output = out)
}


#' get attachment
#'
#' @template submission_id
#' @template token_name
#' @param ... arguments passed to \code{\link[httr]{GET}}
#' @importFrom tools file_ext
#' @return
#' @noRd
fetch_attachment <- function(submission_id, token_name, ...){
  path <- attachments_path(submission_id)
  resp <- lapply(path, nettskjema_api, token_name = token_name, ...)
  invisible(lapply(resp, api_catch_error))
  paths <- file.path(path, unlist(lapply(resp, content)))

  ats <- lapply(1:length(paths), function(x){
    resp <- nettskjema_api(paths[x], token_name, ...)
    api_catch_error(resp)
    at <- content(resp)
    data.frame(
      submission_id = submission_id,
      original = at$fileName,
      standardised = sprintf("%s-%s.%s", submission_id, x, tools::file_ext(at$fileName)),
      path = paths[x],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, ats)
}

#' convenience attachments path.
#' @template submission_id
#' @noRd
attachments_path <- function(submission_id){
  file.path("submissions", submission_id, "attachments")
}
