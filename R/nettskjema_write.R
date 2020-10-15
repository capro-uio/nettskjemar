#' Write meta-data to file
#'
#' Save meta-data information to a
#' file for safe keeping. The function
#' saves the data in json-format, which
#' best preserves the information.
#'
#' @param meta object of class nettskjema_meta class
#' @param file file name or path
#' @param pretty logical. If json-file should be made "pretty"
#' @param ... other arguments to \code{\link[jsonlite]{write_json}}
#'
#' @export
nettskjema_write_meta <- function(meta, file, pretty = TRUE, ...){
  UseMethod("nettskjema_write_meta")
}

#' @export
#' @rdname nettskjema_write_meta
nettskjema_write_meta.default <- function(meta, file, pretty = TRUE, ...){
  warning(paste("Cannot write object of class", class(meta)[1], "as meta-data file"),
          call. = FALSE)
}

#' @export
#' @rdname nettskjema_write_meta
nettskjema_write_meta.nettskjema_meta_raw <- function(meta, file, pretty = TRUE, ...){
  jsonlite::write_json(meta,
                       path = file,
                       pretty = pretty, ...)
}

#' @export
#' @rdname nettskjema_write_meta
nettskjema_write_meta.nettskjema_meta_data <- function(meta, file, pretty = TRUE, ...){
  jsonlite::write_json(meta,
                       path = file,
                       pretty = pretty, ...)
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
#' @param codebook object of class nettskjema_codebook
#' @param pretty logical. If writing json-file, make it pretty
#' @param sep character. If writing text table, column delimiter.
#' @param file filename or path
#' @param ... arguments to \code{\link[jsonlite]{write_json}} or \code{\link[utils]{write.table}}
#'
#' @export
nettskjema_write_codebook <- function(codebook, file, ...){
  UseMethod("nettskjema_write_codebook")
}

#' @export
#' @rdname nettskjema_write_codebook
nettskjema_write_codebook.default <- function(codebook, file, ...){
  warning(paste("Cannot write object of class", class(codebook)[1], "as codebook-data file"),
          call. = FALSE)
}

#' @export
#' @rdname nettskjema_write_codebook
nettskjema_write_codebook.nettskjema_codebook_raw <- function(codebook, file, pretty = TRUE, ...){
  jsonlite::write_json(codebook,
                       path = file,
                       pretty = pretty, ...)
}

#' @export
#' @rdname nettskjema_write_codebook
nettskjema_write_codebook.nettskjema_codebook <- function(codebook, file, sep = "\t", ...){
  utils::write.table(codebook,
              file = file,
              sep = sep,
              row.names = FALSE,
              ...)
}