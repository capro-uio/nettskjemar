# *Required* as vcr is set up on loading
library("vcr")

client_id <- Sys.getenv("NETTSKJEMA_CLIENT_ID")
client_sec <- Sys.getenv("NETTSKJEMA_CLIENT_SECRET")

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "<<CLIENT_ID>>" = client_id,
    "<<CLIENT_SECRET>>" = client_sec,
    "<<BASIC_AUTH>>" = openssl::base64_encode(paste0(
      client_id,
      ":",
      client_sec
    )),
    "<<ACCESS_TOKEN>>" = ns_auth_token()$access_token
  ),
  dir = vcr::vcr_test_path("fixtures")
))

if (!nzchar(Sys.getenv("NETTSKJEMA_CLIENT_ID"))) {
  Sys.setenv("NETTSKJEMA_CLIENT_ID" = "foo")
}

if (!nzchar(Sys.getenv("NETTSKJEMA_CLIENT_SECRET"))) {
  Sys.setenv("NETTSKJEMA_CLIENT_SECRET" = "bar")
}

vcr::check_cassette_names()
