#' Get data from a form
#'
#' This function connects to a specific form and fetches all answers.
#' The responses are placed in a data.frame for easy use.
#'
#' @template form_id
#' @template asis
#' @param labelled Logical. Should labelled data be
#'   returned. Labelled data applies contextual
#'   information from the codebook into special
#'   attributes in the data.frame. See the
#'   \code{labelled} package for more information.
#'
#' @return tibble data.frame
#' @export
#' @examples
#' \dontrun{
#'
#' # Retrieve all data
#' data_110000 <- nskj_get_data(110000)
#'
#' }
nskj_get_data <- function(form_id, asis = FALSE, labelled = !asis) {
  if (asis) {
    if (labelled)
      cli::cli_alert_warning(
        "labels cannot be applied to raw data, ignoring {code labelled = TRUE}"
      )

    resp <- nskj_req() |>
      httr2::req_url_path_append("form", form_id, "answers") |>
      httr2::req_perform(verbosity = 0)

    if (!httr2::resp_has_body(resp)) {
      cli::cli_alert_info("Form has no answers. Returning nothing.")
      return(NULL)
    }

    cont <- httr2::resp_body_string(resp)
    dt <- jsonlite::stream_in(
      textConnection(cont),
      verbose = FALSE
    )
    return(dt)
  }

  dt_csv <- nskj_get_form_reports(
    form_id,
    type = "csv",
    path = NULL
  ) |>
    httr2::resp_body_string()

  dt <- utils::read.csv2(
    textConnection(dt_csv),
    check.names = FALSE
  )

  if (!labelled) return(dt)

  if (labelled && !has_codebook(form_id)) {
    cli::cli_alert_warning(
      "labels can only be used on data with a valid codebook, ignoring {code labelled = TRUE}"
    )
    return(dt)
  }

  cb <- nskj_get_codebook(form_id)
  cols <- cb[, c("element_code", "element_text")] |>
    unique() |>
    stats::na.omit()
  cols <- stats::setNames(cols$element_text, cols$element_code)

  dt <- add_var_labels(dt, cols)
  dt <- add_val_labels(dt, cb)

  return(dt)
}

nskj_get_submissions <- nskj_get_data

#' Get an individual submission answer
#'
#' @template submission_id
#' @return list of all answers
#' @export
#' @examples
#' # example code
#' \dontrun{
#' nskj_get_submission(1100)
#' }
nskj_get_submission <- function(submission_id) {
  resp <- nskj_req() |>
    httr2::req_url_path_append(
      "form",
      "submission",
      submission_id
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  return(resp)
}

#' Get an individual submission answer as a pdf
#'
#' @template submission_id
#' @template path
#' @return nothing, writes file to path
#' @export
#' @examples
#' # example code
#' \dontrun{
#' nskj_get_submission_pdf(1100)
#' nskj_get_submission_pdf(1100, "~/Desktop/1100.pdf")
#' }
nskj_get_submission_pdf <- function(
  submission_id,
  path = sprintf("%s.pdf", submission_id)
) {
  resp <- nskj_req() |>
    httr2::req_url_path_append(
      "form",
      "submission",
      submission_id,
      "pdf"
    ) |>
    httr2::req_perform()

  writeBin(httr2::resp_body_raw(resp), path)
}
