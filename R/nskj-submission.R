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
nskj_get_submission <- function(submission_id){

  resp <- nskj_req() |> 
    httr2::req_url_path_append(
      "form", "submission", submission_id
    ) |> 
    httr2::req_perform() |> 
    httr2::resp_body_json()

  return(resp)
}

#' Get an individual submission answer as a pdf
#' 
#' @template submission_id
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
){

  resp <- nskj_req() |> 
    httr2::req_url_path_append(
      "form", "submission", submission_id, "pdf"
    ) |> 
    httr2::req_perform()

  writeBin(httr2::resp_body_raw(resp), path)
}
