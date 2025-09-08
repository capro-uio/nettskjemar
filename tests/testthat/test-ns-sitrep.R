test_that("ns_sitrep outputs package version and configuration correctly", {
  testthat::local_mocked_bindings(
    ns_url = function() "mock_url",
    ns_has_auth = function() TRUE,
    ns_get_me = function() invisible(),
    ns_validate_client_pattern = function(x, type) TRUE
  )

  # Mock environment variables
  withr::local_envvar(
    NETTSKJEMA_CLIENT_ID = "mock-client-id",
    NETTSKJEMA_CLIENT_SECRET = "mock-client-secret"
  )

  expect_message(
    ns_sitrep(),
    "Nettskjema API Sitrep"
  )

  expect_message(
    ns_sitrep(),
    "mock_url"
  )
  expect_message(
    ns_sitrep(),
    "Client ID: Valid format and length"
  )

  expect_message(
    ns_sitrep(),
    "Client Secret: Valid format and length."
  )

  expect_message(ns_sitrep(), "R version:")
  expect_message(ns_sitrep(), "OS:")
})

test_that("ns_sitrep handles missing API credentials gracefully", {
  testthat::local_mocked_bindings(
    ns_url = function() "mock_url",
    ns_has_auth = function() FALSE,
    ns_validate_client_pattern = function(x, type) FALSE
  )

  withr::local_envvar(
    NETTSKJEMA_CLIENT_ID = NA,
    NETTSKJEMA_CLIENT_SECRET = ""
  )

  expect_message(
    ns_sitrep(),
    "Client ID not found or empty.*"
  )

  expect_message(
    ns_sitrep(),
    "Client Secret not found or empty.*"
  )

  expect_message(
    ns_sitrep(),
    "Cannot test API connectivity.*"
  )

  withr::local_envvar(
    NETTSKJEMA_CLIENT_ID = "bla-bla",
    NETTSKJEMA_CLIENT_SECRET = "blu-blu-blu"
  )

  expect_message(
    ns_sitrep(),
    "Client ID: Invalid format or length."
  )

  expect_message(
    ns_sitrep(),
    "Client Secret: Invalid format or length."
  )

  expect_message(
    ns_sitrep(),
    "Cannot test API connectivity.*"
  )
})

test_that("ns_validate_client_pattern correctly validates UUIDs", {
  expect_true(ns_validate_client_pattern(
    mock_client_id,
    "id"
  ))
  expect_false(ns_validate_client_pattern(NA, "id"))

  expect_false(ns_validate_client_pattern(
    "invalid-uuid",
    "id"
  ))
  expect_false(ns_validate_client_pattern("", "id"))

  expect_true(ns_validate_client_id(mock_client_id))
})

test_that("ns_validate_client_pattern correctly validates secrets", {
  expect_true(ns_validate_client_pattern(
    paste(rep("a", 72), collapse = ""),
    "secret"
  ))
  expect_false(ns_validate_client_pattern(NA, "secret"))

  expect_false(ns_validate_client_pattern(
    paste(rep("a", 71), collapse = ""),
    "secret"
  ))
  expect_false(ns_validate_client_pattern("", "secret"))

  expect_true(ns_validate_client_secret(
    paste(rep("a", 72), collapse = "")
  ))
})

test_that("ns_sitrep handles API connectivity errors", {
  withr::local_tempdir()
  testthat::local_mocked_bindings(
    ns_url = function() "mock_url",
    ns_has_auth = function() TRUE,
    ns_get_me = function() stop("Mock API error")
  )

  # Mock environment
  withr::local_envvar(
    NETTSKJEMA_CLIENT_ID = "mock-client-id",
    NETTSKJEMA_CLIENT_SECRET = "mock-client-secret"
  )

  expect_message(
    ns_sitrep(),
    "Failed to connect "
  )
})
