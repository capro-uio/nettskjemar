test_that("ns_client creates an OAuth2 client correctly", {
  # Test case: Valid inputs
  client <- ns_client(
    client_id = mock_client_id,
    client_secret = mock_client_secret,
    client_name = "test_name"
  )
  expect_s3_class(client, "httr2_oauth_client")
  expect_equal(client$id, mock_client_id)
  expect_equal(client$secret, mock_client_secret)
  expect_equal(client$name, "test_name")
  expect_equal(
    client$token_url,
    "https://authorization.nettskjema.no/oauth2/token"
  )
  expect_equal(
    client$auth,
    "oauth_client_req_auth_header"
  )

  # Test case: Default name
  client_default_name <- ns_client(
    client_id = mock_client_id,
    client_secret = mock_client_secret
  )
  expect_equal(client_default_name$name, "nettskjemar")
})

test_that("ns_req_auth authenticates requests correctly", {
  # Test case: Valid credentials with environment variables
  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = mock_client_id,
      NETTSKJEMA_CLIENT_SECRET = mock_client_secret
    ),
    {
      req <- httr2::request("https://example.com")
      auth_req <- ns_req_auth(req)
      expect_s3_class(auth_req, "httr2_request")
    }
  )

  # Test case: Missing client_id and client_secret
  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = "",
      NETTSKJEMA_CLIENT_SECRET = ""
    ),
    {
      req <- httr2::request("https://example.com")
      expect_error(
        ns_req_auth(req)
      )
    }
  )
})

# Test ns_url
test_that("ns_url returns the correct URL", {
  expect_equal(ns_url(), "https://nettskjema.no/api/v3/")
})

# Test ns_has_auth
test_that("ns_has_auth identifies variables", {
  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = mock_client_id,
      NETTSKJEMA_CLIENT_SECRET = mock_client_secret
    ),
    {
      expect_true(ns_has_auth())
    }
  )

  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = "",
      NETTSKJEMA_CLIENT_SECRET = ""
    ),
    {
      expect_false(ns_has_auth())
    }
  )
})

# Test ns_req with VCR
test_that("ns_req creates a valid request", {
  vcr::use_cassette("ns_req", {
    with_mocked_nettskjema_auth(
      req <- ns_req() |>
        httr2::req_url_path_append("me") |>
        httr2::req_perform()
    )
  })
  expect_s3_class(req, "httr2_response")
  expect_equal(httr2::resp_status(req), 200)
})

test_that("ns_get_token extracts token from request", {
  test_token <- "test_token_123"
  # Mock a successful request with token
  mock_response <- list(
    request = list(
      policies = list(
        auth_sign = list(
          cache = list(get = function() {
            list(access_token = test_token)
          })
        )
      )
    )
  )

  # Mock ns_req and req_perform to return our mock
  local_mocked_bindings(
    "req_url_path_append" = function(req, ...) req,
    "req_perform" = function(req) mock_response,
    .package = "httr2"
  )
  local_mocked_bindings(
    "ns_req" = function(...) list()
  )

  result <- ns_get_token()
  expect_equal(
    result$access_token,
    test_token
  )
})

test_that("ns_get_token handles missing token gracefully", {
  # Mock response with missing token structure
  mock_response <- list(request = list(policies = list()))

  local_mocked_bindings(
    "req_url_path_append" = function(req, ...) req,
    "req_perform" = function(req) mock_response,
    .package = "httr2"
  )
  local_mocked_bindings(
    "ns_req" = function(...) list(),
  )
  expect_error(
    ns_get_token(),
    "Failed to retrieve access token"
  )
})
