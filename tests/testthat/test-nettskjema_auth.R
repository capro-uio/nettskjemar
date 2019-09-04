test_that("token expiry check works", {
  expect_error(nettskjema_token_expiry("bla"),
               "does not exist")

  skip_if_no_auth()
  expect_message(nettskjema_token_expiry(),
               "expires in")

  suppressMessages(
  expect_equal(class(nettskjema_token_expiry()),
                 "Date"))

})
