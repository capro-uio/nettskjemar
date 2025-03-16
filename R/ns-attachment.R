#' Save Nettskjema attachments to file
#'
#' The Nettskjema forms has an option
#' to upload attachments with forms.
#' These can be retrieved by this function.
#' Recommended workflow is to first call
#' \code{\link{ns_list_submission_attachments}},
#' and use the output of this to pass along
#' to the function. This function is called
#' by \code{\link{ns_get_form_attachments}},
#' but you can use it to define your own output
#' file names.
#'
#' @param path Character. Location to save the file.
#' @template attachment_id
#' @return httr2-response. saves attachment to file.
#' @export
#'
#' @examples
#' \dontrun{
#' #submission_id <- c(22222, 1232, 21555)
#' #attach_dt <- ns_list_submission_attachments(submission_id)
#' #ns_save_attachment(path = attach_dt$path, output = attach_dt$standardized)
#' }
ns_get_attachment <- function(attachment_id, path = NULL) {
  resp <- ns_req() |>
    httr2::req_url_path_append(
      "form",
      "submission",
      "attachment",
      attachment_id
    ) |>
    httr2::req_perform()

  if (!is.null(path)) writeBin(httr2::resp_body_raw(resp), path)

  invisible(resp)
}

#' Retrieve all attachments associated with a Nettskjema
#'
#' Some Nettskjema have fields for attachments. These
#' can be downloaded through this function. There
#' are two naming convention that may be used for the
#' saved files' names. See output of
#' \code{\link{ns_list_submission_attachments}} for idea
#' of the names used.
#'
#' @details filenames types
#' \describe{
#'  \item{"original"}{ - uses file names are they were
#'     uploaded to Nettskjema}
#'  \item{"standardized"}{ - creates file names based on
#'     the submission id and a counter number to uniquely
#'     create file names for each submission (in case there
#'     are more than one attachment)}
#'  }
#'
#' @template form_id
#'
#' @return invisible named logical if the file was saved successfully
#' @export
#'
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' ns_list_form_attachments(form_id)
#'
#' }
ns_list_form_attachments <- function(form_id) {
  ids <- ns_get_data(form_id, type = "long")
  ids <- subset(ids, !is.na(ids$answerAttachmentId))
  ids$counter <- stats::ave(
    ids$submissionId,
    ids$submissionId,
    FUN = seq_along
  )
  return(ids)
}

#' Retrieve all attachments associated with a Nettskjema
#'
#' Some Nettskjema have fields for attachments. These
#' can be downloaded through this function. There
#' are two naming convention that may be used for the
#' saved files' names. See output of
#' \code{\link{ns_list_submission_attachments}} for idea
#' of the names used.
#'
#' @details filenames types
#' \describe{
#'  \item{"original"}{ - uses file names are they were
#'     uploaded to Nettskjema}
#'  \item{"standardized"}{ - creates file names based on
#'     the submission id and a counter number to uniquely
#'     create file names for each submission (in case there
#'     are more than one attachment)}
#'  }
#'
#' @template form_id
#' @inheritParams ns_get_submission_attachments
#'
#' @return invisible named logical if the file was saved successfully
#' @export
#'
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' ns_get_form_attachments(form_id)
#'
#' # save files to specific folder
#' ns_get_form_attachments(form_id, output_dir = "~/Desktop")
#'
#' # save using original file names
#' ns_get_form_attachments(form_id, filenames = "original")
#'
#' }
ns_get_form_attachments <- function(
  form_id,
  filenames = c("standardized", "original"),
  output_dir = "."
) {
  filenames <- match.arg(filenames, c("standardized", "original"))

  ids <- ns_list_form_attachments(form_id)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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
      ns_get_attachment,
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
#' submission_id <- 22222
#' ns_list_submission_attachments(submission_id)
#'
#' }
ns_list_submission_attachments <- function(submission_id) {
  submission <- ns_get_submission(submission_id)
  a_idx <- sapply(submission$answers, function(x) {
    x$elementType == "ATTACHMENT"
  })
  a_idx <- which(a_idx)
  ats <- lapply(a_idx, function(x) {
    k <- submission$answers[[x]]
    idx <- which(sapply(k, function(x) {
      length(x) > 0
    }))
    k <- lapply(idx, function(x) k[[x]])
    as.data.frame(k)
  })
  ats <- do.call(rbind, ats)
  ats <- as.data.frame(ats)
  ats$counter <- seq_along(nrow(ats))
  return(ats)
}

#' Get Nettskjema attachments for a submission
#'
#' Each unique submission ID in Nettskjema
#' may have some attachments. This function
#' saves all attachments associated with a
#' submission ID.
#'
#' @template submission_id
#' @param filenames Character of either 'standardized'
#'   (default) or 'original' indicating which file names to use.
#' @param output_dir directory to output the files to
#'
#' @return a data.frame with information on attachments
#'   available, files written to path
#' @export
#' @examples
#' \dontrun{
#' submission_id <- 22222
#' ns_get_submission_attachments(submission_id)
#' ns_get_submission_attachments(submission_id, "original")
#'
#' }
ns_get_submission_attachments <- function(
  submission_id,
  filenames = "standardized",
  output_dir = paste0("./", submission_id)
) {
  ids <- ns_list_submission_attachments(submission_id)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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
      ns_get_attachment,
      ids$answerAttachmentId,
      path = file.path(output_dir, files)
    )
  )
}
