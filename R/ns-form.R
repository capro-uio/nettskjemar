#' Get all forms you have access to
#'
#' All users have access to specific
#' forms, and this function uses the
#' token provided to check what the
#' current user has access to.
#'
#' @template asis
#' @return data.frame, if \code{asis = TRUE} returns a list
#' @export
ns_get_forms <- function(asis = FALSE) {
  resp <- ns_req() |>
    httr2::req_url_path_append("form", "me") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (asis) return(resp)

  list2df(resp)
}


#' Download files associated with a form
#'
#' Each Nettskjema form has several
#' auxiliary forms that can be retrieved.
#'
#' @template form_id
#' @param type Character. Either "csv", "excel" or
#'   "spss".
#' @param path Character. Full path to where the
#'   downloaded file should be saved. Defaults to
#'   current directory with correct extension.
#' @return httr2-response
#' @export
#' @examples
#' \dontrun{
#' ns_get_form_reports(10009, type="csv")
#' ns_get_form_reports(10009, type="excel")
#' ns_get_form_reports(10009, type="spss")
#' ns_get_form_reports(
#'   10009,
#'   type="spss",
#'   path = "~/Desktop/10009/form.sav"
#' )
#' }
ns_get_form_reports <- function(
  form_id,
  type = c("csv", "excel", "spss"),
  path = report_path(form_id, type)
) {
  type <- match.arg(type)

  type <- switch(
    type,
    csv = "csv-report",
    spss = "spss-syntax",
    excel = "excel-report"
  )

  resp <- ns_req() |>
    httr2::req_url_path_append("form", form_id, type) |>
    httr2::req_perform()

  if (!is.null(path)) writeBin(httr2::resp_body_raw(resp), path)

  invisible(resp)
}

#' @noRd
report_path <- function(form_id, type) {
  ext <- switch(type, csv = "csv", spss = "sav", excel = "xlsx")
  sprintf("%s.%s", form_id, ext)
}
