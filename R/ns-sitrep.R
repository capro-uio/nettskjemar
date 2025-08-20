#' Nettskjema API Sitrep
#'
#' This function provides a situational
#' report (sitrep) for the Nettskjema R package.
#' It outputs detailed information on the
#' package version, API configuration,
#' environment
#' variables, and tests API connectivity when credentials are available.
#'
#' The function performs the following:
#' - Displays the installed version of the `nettskjemar` package.
#' - Validates the `NETTSKJEMA_CLIENT_ID`
#' and `NETTSKJEMA_CLIENT_SECRET`
#' environment variables.
#' - Checks the API base URL for
#' connectivity and attempts a test request if
#' credentials are configured.
#' - Displays system environment details such as R version and operating system.
#'
#' @return No return value; the function
#' outputs diagnostic and configuration status
#' messages to the console.
#'
#' @export
#' @examples
#' \dontshow{
#' vcr::insert_example_cassette("ns_sitrep", package = "nettskjemar")
#' nettskjemar:::mock_if_no_auth()
#' }
#'
#' ns_sitrep()
#'
#' \dontshow{
#' vcr::eject_cassette()
#' }
ns_sitrep <- function() {
  cli::cli_h1("Nettskjema API Sitrep")

  # Package Version
  # nolint start
  pkg_version <- utils::packageVersion("nettskjemar")
  # nolint end
  cli::cli_alert_success("nettskjemar version: {.val {pkg_version}}")
  cli::cli_text("")

  cli::cli_h2("Nettskjema API Configuration")

  # API Base URL Check
  cli::cli_alert_info("API URL is set to: {.val {ns_url()}}")

  client_id <- Sys.getenv(
    "NETTSKJEMA_CLIENT_ID",
    unset = NA
  )
  if (validate_client_pattern(client_id, type = "id")) {
    cli::cli_alert_success("Client ID: Valid format and length.")
  } else {
    if (is.na(client_id) || nzchar(client_id)) {
      cli::cli_alert_warning(
        "Client ID not found or empty (check {.envvar NETTSKJEMA_CLIENT_ID})"
      )
    } else {
      cli::cli_alert_danger("Client ID: Invalid format or length.")
      cli::cli_alert_info(
        "Expected format: UUID (e.g., 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx')"
      )
    }
  }

  client_secret <- Sys.getenv(
    "NETTSKJEMA_CLIENT_SECRET",
    unset = NA
  )
  if (validate_client_pattern(client_secret, type = "secret")) {
    cli::cli_alert_success("Client Secret: Valid format and length.")
  } else {
    if (is.na(client_secret) || nchar(client_secret) == 0) {
      cli::cli_alert_warning(
        "Client Secret not found or empty (check 
        {.envvar NETTSKJEMA_CLIENT_SECRET})"
      )
    } else {
      cli::cli_alert_danger(
        "Client Secret: Invalid format or length."
      )
      cli::cli_alert_info("Expected format: 72 alphanumeric characters.")
    }
  }
  cli::cli_text("")

  cli::cli_h2("API Connectivity")

  if (ns_has_auth()) {
    tryCatch(
      {
        invisible(ns_get_me())
        cli::cli_alert_success("Successfully connected to API")
        cli::cli_alert_info(
          "Tested by fetching client profile"
        )
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to connect to Nettskjema 
          API (Error: {.val {e$message}})"
        )
        cli::cli_alert_info(
          "Check your API Key/Token and Base
           URL. Ensure you have network 
           access."
        )
      }
    )
  } else {
    cli::cli_alert_danger(
      "Cannot test API connectivity: API credentials not found."
    )
  }
  cli::cli_text("") # Add a blank line for spacing

  cli::cli_h2("Environment")
  cli::cli_alert_success("R version: {.val {R.version.string}}")
  cli::cli_alert_success(
    "OS: {.val {Sys.info()[['sysname']]}} {.val {Sys.info()[['release']]}}"
  )

  cli::cli_rule()
}

#' Validate Nettskjema Client Pattern
#'
#' Validates the format of Nettskjema client
#'  identifiers such as the `NETTSKJEMA_CLIENT_ID`
#' or `NETTSKJEMA_CLIENT_SECRET`. Returns
#' `TRUE` if the input matches the required
#' format, and `FALSE` otherwise.
#'
#' - `"id"` validation expects a UUID format:
#' `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`.
#' - `"secret"` validation expects a
#' 72-character alphanumeric string.
#'
#' @param x A character string to validate.
#' @param type The identifier type to
#'     validate (`"id"` or `"secret"`).
#' @return A logical value: `TRUE` if the
#'     input format is valid, otherwise
#' @noRd
validate_client_pattern <- function(x, type = c("id", "secret")) {
  type <- match.arg(type, c("id", "secret"))

  # nolint start
  pattern <- switch(
    type,
    "secret" = "^[a-zA-Z0-9]{72}$",
    "id" = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
  )
  # nolint end

  if (
    is.character(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      nchar(x) > 0
  ) {
    return(grepl(pattern, x))
  }

  FALSE
}
