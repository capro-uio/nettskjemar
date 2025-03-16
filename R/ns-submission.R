#' Get an individual submission answer
#'
#' @template submission_id
#' @return list of all answers
#' @export
#' @examples
#' # example code
#' \dontrun{
#' ns_get_submission(1100)
#' }
ns_get_submission <- function(submission_id) {
  resp <- ns_req() |>
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
#' ns_get_submission_pdf(1100)
#' ns_get_submission_pdf(1100, "~/Desktop/1100.pdf")
#' }
ns_get_submission_pdf <- function(
  submission_id,
  path = sprintf("%s.pdf", submission_id)
) {
  resp <- ns_req() |>
    httr2::req_url_path_append(
      "form",
      "submission",
      submission_id,
      "pdf"
    ) |>
    httr2::req_headers("accept: application/pdf") |>
    httr2::req_perform()

  writeBin(httr2::resp_body_raw(resp), path)
}
