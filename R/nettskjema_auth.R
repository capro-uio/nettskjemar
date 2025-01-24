
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

  req <- httr2::request(
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
    )
  
  if(cache){
    req <- req |> 
      httr2::req_cache(
        cache_path,
        max_age = 24*60*60
      )
  }

  req |> 
    httr2::req_perform() |> 
    httr2::resp_body_json()
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

  