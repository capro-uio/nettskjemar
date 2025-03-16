#' Get data from a form
#'
#' This function connects to a specific form and fetches all answers.
#' The responses are placed in a data.frame for easy use.
#'
#' \describe{
#'  \item{"original"}{ - Returns data in the same tabular format
#'      as delivered by Nettskjema.}
#' \item{"labelled"}{ - Returns labelled data, where the
#'      codebook is minimally integrated into the data,
#'      similar to SPSS or Stata.}
#' \item{"long"}{ - Returns the data in tall format, where
#'      there are multiple rows per participant (one per question)
#'      and each choice is timestamped.}
#' }
#'
#' @template form_id
#' @param type Character or NULL. One of either "original",
#'      "labelled" or "long". If NULL, will return "labelled" data
#'      if the form has the codebook set up, else will return
#'      "original."
#'
#' @return tibble data.frame
#' @export
#' @examples
#' \dontrun{
#'
#' # Retrieve all data
#' data_110000 <- ns_get_data(110000)
#'
#' }
ns_get_data <- function(
  form_id,
  type = NULL
) {
  type <- match.arg(type, c("original", "labelled", "long"))
  if (is.null(type)) {
    type <- ifelse(
      !has_codebook(form_id),
      "original",
      "labelled"
    )
  }

  if (type == "long") {
    resp <- ns_req() |>
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
    dt <- cbind(
      formid = rep(form_id, nrow(dt)),
      dt
    )
    return(dt)
  }

  dt_csv <- ns_get_form_reports(
    form_id,
    type = "csv",
    path = NULL
  ) |>
    httr2::resp_body_string()

  dt <- utils::read.csv2(
    textConnection(dt_csv),
    check.names = FALSE
  )
  dt <- cbind(
    formid = rep(form_id, nrow(dt)),
    dt
  )

  if (type == "original") return(dt)

  if (type == "labelled" && !has_codebook(form_id)) {
    cli::cli_alert_warning(
      "labels can only be used on data with a valid
      codebook, ignoring {code labelled = TRUE}"
    )
    return(dt)
  }

  add_labels(dt, ns_get_codebook(form_id))
}

#' @export
#' @describeIn ns_get_data Alternative function name, does the same.
ns_get_submissions <- ns_get_data
