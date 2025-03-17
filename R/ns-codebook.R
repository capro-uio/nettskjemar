#' Constructor for nettskjema codebook
#'
#' Function works on meta-data downloaded from a form.
#' It will return a data.frame in long format with
#' meta-data information from a specific nettskjema form.
#'
#' @param meta form meta-data from \code{\link{ns_get_meta}}
#' @return long format tibble with meta-data for a specific nettskjema
#' @export
#'
#' @examples
#' \dontrun{
#' meta_110000 <- ns_get_meta(110000)
#' codebook(meta_110000)
#' }
codebook <- function(meta) {
  els <- lapply(
    seq_along(meta$elements),
    .el2df,
    details = meta
  )
  els <- do.call(rbind, els)

  els$subelement_text <- NULL
  els$subelement_code <- NULL

  structure(
    els,
    class = c("ns_codebook", "data.frame")
  )
}

#' Get codebook for a form
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#'
#' @template form_id
#' @template asis
#'
#' @return list of class ns_meta_data
#' @export
#' @examples
#' \dontrun{
#' codebook_110000 <- ns_get_codebook(110000)
#' }
ns_get_codebook <- function(form_id, asis = FALSE) {
  cb <- get_raw_codebook(
    form_id = form_id
  )

  if (asis) return(cb)

  codebook(cb)
}


#' Get the raw codebook
#'
#' The raw codebook is a list
#' of answers and questions with
#' unique ids for both, and the text associated
#' with them. This information can be retrieved
#' from the raw nettskjema data. This can be useful
#' if the nettskjema submission data has been returned
#' with \code{asis = TRUE}.
#' @template form_id
#' @return long format tibble with meta-data for a specific nettskjema
#' @examples
#' \dontrun{
#' form_id <- 100000
#' get_raw_codebook(form_id)
#'
#' Or if the token is saved in a non-standard name
#' get_raw_codebook(form_id)
#' }
get_raw_codebook <- function(form_id) {
  resp <- ns_req() |>
    httr2::req_url_path_append("form", form_id, "elements") |>
    httr2::req_perform()

  structure(
    list(
      form_id = form_id,
      elements = httr2::resp_body_json(resp)
    ),
    class = c("ns_codebook_raw", "list")
  )
}

#' @export
format.ns_codebook_raw <- function(x, ...) {
  c(
    sprintf("# Nettskjema raw codebook for form %s", x$form_id),
    "",
    sprintf("no. elements: %s", length(x$elements)),
    print(
      sapply(x$elements, function(y) {
        y$elementType
      }),
      ...
    )
  )
}

#' @export
print.ns_codebook_raw <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}


#' Write codebook to file
#'
#' Save codebook information to a
#' file for safe keeping. Depending
#' on the type of codebook (raw or tidy)
#' the file will either be stored as a
#' json-file through \code{\link[jsonlite]{write_json}}
#' or a text table through \code{\link[utils]{write.table}}.
#'
#' @details Given the two types of codebooks, writes different
#'        types of files.
#' \describe{
#' \item{codebook}{ - writes a tab-separated table}
#' \item{codebook_raw}{ - writes a json-file}
#' }
#'
#' @param codebook object of class ns_codebook
#' @param sep character. If writing text table, column delimiter.
#' @param path filename or path
#' @param ... arguments to
#'     \code{\link[jsonlite]{write_json}} or
#'     \code{\link[utils]{write.table}}
#' @return no return value. Writes a file to path.
#' @export
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' my_cb <- ns_get_codebook(form_id)
#' ns_write_codebook(my_cb, "my/path/codebook_110000.txt")
#' }
ns_write_codebook <- function(codebook, path, ...) {
  UseMethod("ns_write_codebook")
}

#' @export
#' @rdname ns_write_codebook
ns_write_codebook.default <- function(codebook, path, ...) {
  warning(
    "Cannot write object of class",
    class(codebook)[1],
    "as codebook-data file",
    call. = FALSE
  )
}

#' @export
#' @rdname ns_write_codebook
ns_write_codebook.ns_codebook_raw <- function(codebook, path, ...) {
  if (!grepl("json$", path)) {
    message("Switching path extension to .json")
    path <- sprintf("%s.json", rm_ext(path))
  }
  jsonlite::write_json(
    codebook,
    path = path,
    ...
  )
}

#' @export
#' @rdname ns_write_codebook
ns_write_codebook.ns_codebook <- function(codebook, path, sep = "\t", ...) {
  utils::write.table(
    codebook,
    file = path,
    sep = sep,
    row.names = FALSE,
    ...
  )
}

#' Check if form has codebook
#'
#' Codebook is by default turned off
#' in Nettskjema, but best practices in handling
#' data is to have it on. This function
#' checks if the codebook of a form has been activated
#' or not
#'
#' @template form_id
#'
#' @return logical is codebook is turned on
#' @export
#' @importFrom tools file_ext
#' @examples
#' \dontrun{
#' has_codebook(110000)
#' }
has_codebook <- function(form_id) {
  meta <- ns_get_meta(form_id)
  meta$isCodebookValid
}

#' @noRd
.el2df <- function(id, details) {
  element <- details$elements[[id]]
  els <- data.frame(
    element_no = id,
    element_type = element$elementType %||% NA,
    element_code = element$externalElementId %||% NA,
    element_text = element$text %||% NA,
    element_desc = element$description %||% NA
  )

  if (
    length(element$subElements) == 0 &&
      length(element$answerOptions) == 0
  ) {
    answ <- data.frame(
      subelement_text = NA,
      subelement_code = NA,
      subelement_seq = NA,
      answer_text = NA,
      answer_code = NA,
      answer_seq = NA
    )
    return(cbind(els, answ))
  }

  answ <- lapply(element$answerOptions, function(a) {
    data.frame(
      element_no = id,
      answer_text = a$text %||% NA,
      answer_code = a$externalAnswerOptionId %||% NA,
      answer_seq = a$sequence %||% NA
    )
  })
  answ <- do.call(rbind, answ)

  if (length(element$subElements) == 0) {
    sube <- data.frame(
      element_no = id,
      subelement_text = NA,
      subelement_code = NA,
      subelement_seq = NA
    )
    sube <- merge_el(sube, answ)
    els <- merge_el(els, sube)

    if (element$elementType == "CHECKBOX") {
      els$element_code <- paste(
        els$element_code,
        els$answer_code,
        sep = "."
      )

      els$element_text <- paste(
        els$element_text,
        els$answer_text,
        sep = ":: "
      )
    }
    return(els)
  }

  sube <- lapply(element$subElements, function(a) {
    data.frame(
      element_no = id,
      subelement_text = a$text %||% NA,
      subelement_code = a$externalElementId %||% NA,
      subelement_seq = a$sequence %||% NA
    )
  })
  sube <- do.call(rbind, sube)
  sube <- merge_el(sube, answ)
  els <- merge_el(els, sube)

  if (element$elementType == "MATRIX_CHECKBOX") {
    els$element_code <- paste(
      els$element_code,
      els$subelement_code,
      els$answer_code,
      sep = "."
    )

    els$element_text <- paste(
      els$element_text,
      els$subelement_text,
      els$answer_text,
      sep = " :: "
    )
  } else {
    els$element_code <- ifelse(
      is.na(els$subelement_code),
      els$element_code,
      paste(els$element_code, els$subelement_code, sep = ".")
    )

    els$element_text <- ifelse(
      is.na(els$subelement_text),
      els$element_text,
      paste(els$element_text, els$subelement_text, sep = ": ")
    )
  }
  els
}
