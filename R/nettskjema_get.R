#' Get data from a form
#'
#' @param form_id integer. Number of the form to retrieve
#' @param use_codebook logical. Use codebook for answers, or
#' retrieve actualy text options.
#' data from
#' @param token_name character. Name to give the token, defaults to 'NETTSKJEMA_API_TOKEN'
#' @param ... arguments passed to httr::GET
#'
#' @return tibble data.frame
#' @export
nettskjema_get_data <- function(form_id,
                                use_codebook = TRUE,
                                token_name = "NETTSKJEMA_API_TOKEN", ...){

  path = paste0("forms/", form_id, "/submissions")

  resp <- nettskjema_api(path, token_name = token_name, ...)

  api_catch_error(resp)

  cont <- httr::content(resp)

  # Add form_id to the outputed data
  dt <- dplyr::mutate(clean_form_submissions(cont),
                      form_id = form_id)
  dplyr::select(dt, form_id, dplyr::everything())
}

#' Get all forms you have access to
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#' TODO: make this work
#'
#' @inheritParams nettskjema_get_data
#'
#' @return list
nettskjema_get_forms <- function(token_name = "NETTSKJEMA_API_TOKEN", ...){

  resp <- nettskjema_api("forms/", token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  httr::content(resp)

}

#' Get metadata for a form
#'
#' With the given API token, will retrieve
#' a list of all the forms you have access to
#'
#' @inheritParams nettskjema_get_forms
#' @inheritParams nettskjema_get_data
#' @param information which meta data elements to extract
#' @param ...  arguments to httr::GET
#'
#' @return list
#' @export
nettskjema_get_meta <- function(form_id,
                                information = c("title", "language",
                                                "created", "modified", "opened",
                                                "respondents", "contact","codebook",
                                                "personal_data", "sensitive_data",
                                                "editors", "elements"),
                                token_name = "NETTSKJEMA_API_TOKEN",
                                ...){

  path = paste0("forms/", form_id)
  resp <- nettskjema_api(path, token_name = token_name, ...)

  api_catch_error(resp)
  api_catch_empty(resp)

  information <- match.arg(information,
                           c("title", "language",
                             "created", "modified", "opened",
                             "respondents", "contact","codebook",
                             "personal_data", "sensitive_data",
                             "editors", "elements"),
                           several.ok = TRUE)

  fields_idx <- meta_fields() %in% information
  fields <- names(meta_fields())[fields_idx]

  cont <- httr::content(resp)

  dt <- lapply(fields, function(x) cont[[x]])
  names(dt) <- meta_fields()[fields_idx]
  meta_classes(dt)
}