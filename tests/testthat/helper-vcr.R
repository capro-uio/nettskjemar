# *Required* as vcr is set up on loading
library("vcr")

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "<<CLIENT_ID>>" = Sys.getenv("NETTSKJEMA_CLIENT_ID"),
    "<<CLIENT_SECRET>>" = Sys.getenv("NETTSKJEMA_CLIENT_SECRET")
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
