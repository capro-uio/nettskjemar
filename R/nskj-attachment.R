
#' Retrieve attachments associated with a Nettskjema
#'
#' Some Nettskjema have fields for attachments. These
#' can be downloaded through this function. There
#' are two naming convention that may be used for the
#' saved files' names. See output of
#' \code{\link{nskj_list_attachments}} for idea
#' of the names used.
#'
#' @details filenames types
#' \itemize{
#'  \item{"original"}{ - uses file names are they were
#'     uploaded to Nettskjema}
#'  \item{"standardized"}{ - creates file names based on
#'     the submission id and a counter number to uniquely
#'     create file names for each submission (in case there
#'     are more than one attachment)}
#'  }
#'
#' @template form_id
#' @param filenames string of either 'standardized' (default) or
#'     'original' indicating which file names to use.
#' @param output_dir directory to output the files to
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return invisible named logical if the file was saved successfully
#' @export
#'
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' nskj_get_form_attachments(form_id)
#'
#' # save files to specific folder
#' nskj_get_form_attachments(form_id, output_dir = "~/Desktop")
#'
#' # save using original file names
#' nskj_get_form_attachments(form_id, filenames = "original")
#'
#' }
nskj_get_form_attachments <- function(
  form_id,
  filenames = c("standardized", "original"),
  output_dir = ".",
  ...){
  filenames <- match.arg(filenames, c("standardized", "original"))

  if(!dir.exists(output_dir)) 
    dir.create(output_dir, recursive = TRUE)
  
  ids <- nskj_get_data(form_id, asis = TRUE)
  ids <- subset(ids, !is.na(ids$answerAttachmentId))
  ids$counter <- stats::ave(
    ids$submissionId, 
    ids$submissionId, 
    FUN = seq_along
  )

  files <- switch(
    filenames,
    "standardized" = sprintf(
      "%s_%03d.%s",
      ids[["submissionId"]],
      ids[["counter"]],
      tools::file_ext(ids[["filename"]])
  ),
    "original" = ids[["filename"]]
  )

  invisible(
    mapply(
      nskj_get_attachment,
      ids$answerAttachmentId, 
      path = file.path(output_dir, files)
    )
  )
}

#' List Nettskjema attachments for a submission
#'
#' Each unique submission ID in Nettskjema
#' may have some attachments. These can be listed
#' using this function, which will provide the
#' information on where in the Nettskjema API the
#' files are located, their original file names,
#' and a suggested standardized file names for
#' tidier data output.
#'
#' @template submission_id
#'
#' @return a data.frame with information on attachments available.
#' @export
#' @examples
#' \dontrun{
#' submission_id <- c(22222, 1232, 21555)
#' nskj_list_attachments(submission_id)
#'
#' }
nskj_list_attachments <- function(submission_id){
  submission <- nskj_get_submission(submission_id)
  a_idx <- sapply(submission$answers, function(x){
    x$elementType == "ATTACHMENT"
  })
  a_idx <- which(a_idx)
  ats <- lapply(a_idx, function(x){
    k <- submission$answers[[x]]
    idx <- which(sapply(k, function(x){ length(x) > 0}))
    k <- lapply(idx, function(x) k[[x]])
    as.data.frame(k)
  })
  ats <- do.call(rbind, ats)
  as.data.frame(ats)
}

#' Save Nettskjema attachments to file
#'
#' The Nettskjema forms has an option
#' to upload attachments with forms.
#' These can be retrieved by this function.
#' Recommended workflow is to first call
#' \code{\link{nskj_list_attachments}},
#' and use the output of this to pass along
#' to the function. This function is called
#' by \code{\link{nskj_get_form_attachments}},
#' but you can use it to define your own output
#' file names.
#'
#' @param path Nettskjema API path where the attachment is
#' @template attachment_id
#' @return nothing. saves attachments to file.
#' @export
#'
#' @examples
#' \dontrun{
#' #submission_id <- c(22222, 1232, 21555)
#' #attach_dt <- nskj_list_attachments(submission_id)
#' #nskj_save_attachment(path = attach_dt$path, output = attach_dt$standardized)
#' }
nskj_get_attachment <- function(attachment_id, path){
  resp <- nskj_req() |> 
    httr2::req_url_path_append(
      "form", "submission", "attachment", attachment_id
    ) |> 
    httr2::req_perform()

  writeBin(httr2::resp_body_raw(resp), path)
}

