
#' Get metadata for a form
#'
#' With the given API token, will retrieve
#' the meta-data connected to a form.
#'
#' @template form_id
#' @template token_name
#' @template as_is
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return list of class nettskjema_meta_data
#' @export
#' @importFrom httr content
#' @examples
#' \dontrun{
#' meta_110000 <- nettskjema_get_meta(110000)
#'
#' }
nettskjema_get_meta <- function(form_id,
                                as_is = FALSE,
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  path = file.path("forms", form_id)
  resp <- nettskjema_api(path, token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  fields_idx <- meta_fields() %in% valid_form_inf()
  fields <- names(meta_fields())[fields_idx]

  cont <- content(resp)

  if(as_is) return(meta_raw(cont))

  dt <- lapply(fields, function(x) cont[[x]])
  names(dt) <- meta_fields()[fields_idx]

  dt$form_id = form_id
  meta_classes(dt)
}


#' @noRd
meta_fields <- function(){
  c(title = "title",
    languageCode = "language",
    createdDate = "created",
    modifiedDate = "modified",
    openFrom = "opened",
    respondentGroup = "respondents",
    editorsContactEmail = "contact",
    codebookActivated = "codebook",
    collectsPersonalData = "personal_data",
    sensitivePersonalDataCollected = "sensitive_data",
    editors = "editors",
    elements = "elements")
}

#' @noRd
meta_classes <- function(content){
  nm <- names(content)

  dt <- lapply(nm, function(x) meta_change_class(x, content))
  names(dt) <- nm
  structure(dt,
            class = c("nettskjema_meta_data", "list"))
}

#' @noRd
meta_change_class <- function(name, content){
  switch(name,
         # characters
         "title" = as.character(content[[name]]),
         "language" = as.character(content[[name]]),
         "respondents" = as.character(content[[name]]),
         "contact"  = as.character(content[[name]]),

         # integer
         "form_id" = as.integer(content[[name]]),

         # dates
         "created" = as.Date(content[[name]],
                             origin = "1970-01-01"),
         "modified" = as.Date(content[[name]],
                              origin = "1970-01-01"),
         "opened" = as.Date(content[[name]],
                            origin = "1970-01-01"),

         # boolean
         "codebook" = as.logical(content[[name]]),
         "personal_data" = as.logical(content[[name]]),
         "sensitive_data" = as.logical(content[[name]]),

         # user tibbles
         "editors" = as_user(content[[name]]),
         "creator" = as_user(content[[name]]),
         "modifier" = as_user(content[[name]]),

         # form elements
         "elements" = as_element(content[[name]])
  )
}

#' @export
format.nettskjema_meta_data <- function(x, ...){
  c(
    sprintf("# Nettskjema metadata for form %s", x$form_id),
    "",
    unname(sapply(c("title","language","opened", "respondents", "contact",
             "codebook", "personal_data", "sensitive_data"),
           function(i) sprintf("%s: %s", i, x[[i]]))
           ),

    sprintf("editors: %s", nrow(x$editors)),
    sprintf("no. elements: %s", length(x$elements$type))
  )
}

#' @export
print.nettskjema_meta_data <- function(x, ...){
  cat(format(x), sep="\n")
  invisible(x)
}

#' @noRd
meta_raw <- function(content){
  structure(content,
            class = c("nettskjema_meta_raw", "list"))
}

#' @export
format.nettskjema_meta_raw <- function(x, ...){
  c(
    sprintf("# Nettskjema raw metadata for form %s", x$formId),
    "",
    unname(sapply(c("title","languageCode","createdDate", "respondentGroup", "editorsContactEmail",
                    "codebookActivated", "collectsPersonalData","sensitivePersonalDataCollected"),
                  function(i) sprintf("%s: %s", i, x[[i]]))
    ),

    sprintf("editors: %s", length(x$editors)),
    sprintf("no. elements: %s", length(x$elements))
  )
}

#' @export
print.nettskjema_meta_raw <- function(x, ...){
  cat(format(x), sep="\n")
  invisible(x)
}


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
#' @return no return value. Writes a file to path.
#' @export
#' @examples
#' \dontrun{
#' form_id <- 1100000
#' my_meta <- nettskjema_get_meta(form_id)
#' nettskjema_write_meta(my_meta, "my/path/meta_110000.json")
#' }
nettskjema_write_meta <- function(meta, file, pretty = TRUE, ...){
  UseMethod("nettskjema_write_meta")
}

#' @export
#' @rdname nettskjema_write_meta
nettskjema_write_meta.default <- function(meta, file, pretty = TRUE, ...){
  warning("Cannot write object of class", class(meta)[1], "as meta-data file",
          call. = FALSE)
}

#' @export
#' @rdname nettskjema_write_meta
nettskjema_write_meta.nettskjema_meta_raw <- function(meta, file, pretty = TRUE, ...){
  if(!grepl("json$", file)){
    message("Switching file extention to .json")
    file <- sprintf("%s.json", rm_ext(file))
  }
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



