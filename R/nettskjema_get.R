nettskjema_get_data <- function(formID, token_name = "NETTSKJEMA_API_TOKEN"){

  path = paste0("forms/", formID)

  resp <- nettskjema_api(path, token_name = token_name)

  api_catch_error(resp)

}