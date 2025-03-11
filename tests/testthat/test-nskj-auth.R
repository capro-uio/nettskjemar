test_that("test request setup", {
  vcr::use_cassette("auth", {
    auth <- nskj_req()
  })

  expect_is(auth, "httr2_request")
  expect_equal(
    names(auth),
    c("url", "method", "headers", "body", "fields", "options", "policies")
  )
  expect_null(auth$body)
  expect_match(auth$url, "https://api.nettskjema.no/v3")
  expect_match(names(auth$headers), "Authorization")
})

test_that("test token setup", {
  vcr::use_cassette("token", {
    token <- nskj_auth_token()
  })

  expect_is(token, "list")
  expect_equal(
    names(token),
    c("access_token", "token_type", "expires_in")
  )
  expect_match(token$token_type, "Bearer")

  # TODO: how to test chaching?
})
