
#' Create Nettskjema API user
#'
#' Opens OS browser to create API user.
#' @param ip_version IP version to look up. Either "v4" (default) or "v6".
#' @return No return value, opens a browser for creating a API user.
#' @importFrom utils browseURL
#' @export
#' @examples
#' \dontrun{
#' nettskjema_user_create()
#'
#' # Turn off ip detection
#' nettskjema_user_create(ip = FALSE)
#' }
nettskjema_user_create <- function(ip_version = c("v4", "v6")){
  nettskjema_find_ip(ip_version)
  browseURL("https://nettskjema.uio.no/user/api/index.html")
}

#' @template token
nettskjema_req <- function(...){
  httr2::request("https://api.nettskjema.no/v3") |> 
    httr2::req_auth_bearer_token(
      nettskjema_auth_token(...)$access_token
    )
}


nettskjema_auth_token <- function(
  client_id = Sys.getenv("NETTSKJEMA_CLIENT_ID"),
  client_secret = Sys.getenv("NETTSKJEMA_CLIENT_SECRET"), 
  cache = TRUE,
  cache_path = fs::path_home(".nettskjema_token.rds")){
  
  if(cache){
    if(file.exists(cache_path)){
      token <- readRDS(cache_path)
      if(Sys.time() < token$expires)
        return(token)
    }
  }

  token <- httr2::request(
    "https://authorization.nettskjema.no/oauth2/token"
    ) |> 
    httr2::req_method("POST") |> 
    httr2::req_body_raw(
      "grant_type=client_credentials", 
      "application/x-www-form-urlencoded"
    ) |> 
    httr2::req_auth_basic(
      client_id, 
      client_secret
    ) |> 
    httr2::req_perform() |> 
    httr2::resp_body_json()

  token$expires <- Sys.time() + token$expires_in
  
  saveRDS(token, cache_path)
  
  return(token)
}

nettskjema_token_expiry <- function(){}

#' Find your current IP
#'
#' The Nettskjema v2 API requires a token
#' to be registered with IP addresses.
#' This function returns the current IP address.
#'
#' If you are working through a VPN, the IP address
#' will not be returned as the Nettskjema API sees it.
#' We are looking for solutions to this.
#'
#' @param version string. ip version to check.
#'   one of either "v4" or "v6".
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @export
#' @examples
#' nettskjema_find_ip()
nettskjema_find_ip <- function(version = c("v4","v6")){
  version <- match.arg(version, c("v4","v6"))
  ip_url <- switch(version,
                   "v4" = "https://api.ipify.org?format=json",
                   "v6" = "https://api64.ipify.org?format=json")
  resp <- GET(ip_url)
  message(
    sprintf(
      "Your current IP%s address is:\n%s",
      version,
      fromJSON(content(resp, "text",
                       encoding = "UTF-8"))$ip
    )
  )
}

  