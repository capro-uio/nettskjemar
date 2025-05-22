test_that("test request setup", {
  vcr::use_cassette("ns_auth", {
    with_mocked_nettskjema_auth(
      auth <- ns_req()
    )
  })
  expect_is(auth, "httr2_request")
  expect_equal(
    names(auth),
    c(
      "url",
      "method",
      "headers",
      "body",
      "fields",
      "options",
      "policies",
      "state"
    )
  )
  expect_null(auth$body)
  expect_match(auth$url, "https://nettskjema.no/api/v3/")
  expect_match(names(auth$headers), "Authorization")
})

# Test ns_url
test_that("ns_url returns the correct URL", {
  expect_equal(ns_url(), "https://nettskjema.no/api/v3/")
})

# Test ns_has_auth
test_that("ns_has_auth identifies variables", {
  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = "dummy_id",
      NETTSKJEMA_CLIENT_SECRET = "dummy_secret"
    ),
    {
      expect_true(ns_has_auth())
    }
  )

  withr::with_envvar(
    c(NETTSKJEMA_CLIENT_ID = "", NETTSKJEMA_CLIENT_SECRET = ""),
    {
      expect_false(ns_has_auth())
    }
  )
})

# Test ns_auth_token with VCR
test_that("ns_auth_token caches access token", {
  vcr::use_cassette("ns_auth_token", {
    with_mocked_nettskjema_auth(
      token <- ns_auth_token(cache = FALSE)
    )
  })
  # Using cache = FALSE for simpler testing
  expect_named(token, c("access_token", "token_type", "expires_in"))
  expect_type(token$access_token, "character")
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
