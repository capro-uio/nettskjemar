#' Get metadata for a form
#'
#' With the given API token, will retrieve
#' the meta-data connected to a form.
#'
#' @template form_id
#' @return list of class ns_meta_data
#' @export
#' @examples
#' \dontshow{
#' vcr::insert_example_cassette("ns_get_meta", package = "nettskjemar")
#' nettskjemar:::mock_auth()
#' }
#' ns_get_meta(123823)
#' \dontshow{
#' vcr::eject_cassette()
#' }
ns_get_meta <- function(form_id) {
  resp <- ns_req() |>
    httr2::req_url_path_append("form", form_id, "info") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  meta_raw(c("form_id" = form_id, resp))
}

#' @noRd
meta_raw <- function(content) {
  structure(content, class = c("ns_meta", "list"))
}

#' @export
format.ns_meta <- function(x, ...) {
  c(
    sprintf("# Nettskjema raw metadata for form %s", x$form_id),
    "",
    unname(sapply(
      c(
        "title",
        "editorsContactEmail",
        "isOpen",
        "isCodebookValid",
        "numberOfSubmissions",
        "modifiedDate"
      ),
      function(i) sprintf("%s: %s", i, x[[i]])
    ))
  )
}


#' Write meta-data to file
#'
#' Save meta-data information to a
#' file for safe keeping. The function
#' saves the data in json-format, which
#' best preserves the information.
#'
#' @param meta object of class ns_meta class
#' @param path file name or path
#' @param ... other arguments to \code{\link[jsonlite]{write_json}}
#' @return no return value. Writes a file to path.
#' @export
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' my_meta <- ns_get_meta(form_id)
#' ns_write_meta(my_meta, "my/path/meta_110000.json")
#' }
ns_write_meta <- function(meta, path, ...) {
  UseMethod("ns_write_meta")
}

#' @export
#' @rdname ns_write_meta
ns_write_meta.default <- function(meta, path, ...) {
  warning(
    "Cannot write object of class",
    class(meta)[1],
    "as meta-data file",
    call. = FALSE
  )
}

#' @export
#' @rdname ns_write_meta
ns_write_meta.ns_meta <- function(meta, path, ...) {
  if (!grepl("json$", path)) {
    cli::cli_alert_info("Switching file extension to .json")
    path <- sprintf("%s.json", rm_ext(path))
  }
  jsonlite::write_json(
    meta,
    path = path,
    ...
  )
}
