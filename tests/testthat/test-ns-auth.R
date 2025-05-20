test_that("ns_auth_token retrieves and caches the token", {
  vcr::use_cassette("ns_auth_token_fetch", {
    token <- ns_auth_token(cache = FALSE)
  })
  # # Assertions on token
  # expect_type(token, "list")
  # expect_true(!is.null(token$access_token))
  # expect_true(!is.null(token$expire_time))

  # # Check expiration time validity
  # expect_gt(
  #   as.numeric(token$expire_time - Sys.time()),
  #   0
  # )
})

test_that("ns_auth_token uses cached token if valid", {
  # Create a temporary cache file
  temp_cache <- tempfile(fileext = ".rda")
  valid_token <- list(
    access_token = "mock_access_token",
    expires_in = (24 * 60 * 60) - 1
  )
  saveRDS(valid_token, file = temp_cache)

  # Call the function with caching enabled
  token <- ns_auth_token(cache = TRUE, cache_path = temp_cache)

  # # Assertions on token
  # expect_type(token, "list")
  # expect_equal(token$access_token, "mock_access_token")
})

test_that("ns_auth_token fetches new token", {
  # Create a temporary cache file with an expired token
  temp_cache <- tempfile(fileext = ".rda")
  expired_token <- list(
    access_token = "expired_access_token",
    expires_in = (24 * 60 * 60) - 1
  )
  saveRDS(expired_token, file = temp_cache)

  vcr::use_cassette("ns_auth_token_fetch_new", {
    token <- ns_auth_token(
      cache = TRUE,
      cache_path = temp_cache
    )
  })

  # Assertions on token: should fetch a new token
  expect_type(token, "list")
  expect_true(token$access_token != "expired_access_token")
  expect_true(!is.null(token$expires_in))
  expect_type(token$expires_in, "integer")
})

test_that("test token setup", {
  vcr::use_cassette("ns_token", {
    token <- ns_auth_token()

    expect_is(token, "list")
    expect_equal(
      names(token),
      c("access_token", "token_type", "expires_in")
    )
    expect_match(token$token_type, "Bearer")
    expect_equal(token$expires_in, (24 * 60 * 60) - 1)
  })
})

# Testing framework for 'ns_has_auth'
test_that("ns_has_auth when FALSE", {
  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = "",
      NETTSKJEMA_CLIENT_SECRET = ""
    ),
    {
      expect_silent({
        expect_false(ns_has_auth())
      })
      expect_error(ns_auth_token(), "not set up")
    }
  )
})

test_that("ns_has_auth when TRUE", {
  withr::with_envvar(
    c(
      NETTSKJEMA_CLIENT_ID = "a1b2c3d4-e5f6-7890-1234-567890abcdef
",
      NETTSKJEMA_CLIENT_SECRET = "test_client_secret"
    ),
    {
      expect_silent({
        expect_true(ns_has_auth())
      })
    }
  )
})


test_that("test request setup", {
  vcr::use_cassette("ns_auth", {
    auth <- ns_req()
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
