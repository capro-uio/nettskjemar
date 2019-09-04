#' Get data from a form
#'
#' @param form_id integer. Number of the form to retrieve
#' @param use_codebook logical. Use codebook for answers, or
#' retrieve actualy text options.
#' data from
#' @inheritParams nettskjema_token2renviron
#'
#' @return tibble data.frame
#' @export
nettskjema_get_data <- function(form_id, use_codebook = TRUE, token_name = "NETTSKJEMA_API_TOKEN"){

  path = paste0("forms/", form_id, "/submissions")

  resp <- nettskjema_api(path, token_name = token_name)

  api_catch_error(resp)

  cont <- httr::content(resp)

  dt <- dplyr::mutate(clean_form_submissions(cont),
         form_id = form_id)
  dplyr::select(dt, form_id, dplyr::everything())
}

#' Get all forms you have access to
#'
#' With the fiven API token, will retrieve
#' a list of all the forms you have access to
#'
#' @inheritParams nettskjema_token2renviron
#'
#' @return list
#' @export
nettskjema_get_forms <- function(token_name = "NETTSKJEMA_API_TOKEN"){

  resp <- nettskjema_api("users/admin/users", token_name = token_name)

  api_catch_error(resp)

  httr::content(resp)

}