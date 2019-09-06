# test_token <- "NETTSKJEMA_TEST_TOKEN"
# test_form <- 123823
#
# test_that("token expiry check works", {
#   expect_error(nettskjema_token_expiry("bla"),
#                "does not exist")
#
#   skip_if_no_auth()
#   expect_message(nettskjema_token_expiry(test_token),
#                  "expires in")
#
#   suppressMessages(
#     expect_equal(class(nettskjema_token_expiry(test_token)),
#                  "Date"))
#
# })
#
# test_that("nettskjema_token2renviron works",{
#
#   # Get path to Renviron to check it's being written to
#   Renviron_path <- usethis:::scoped_path_r(c("user", "project"),
#                                            ".Renviron",
#                                            envvar = "R_ENVIRON_USER")
#
#   # test create
#   expect_message(nettskjema_token2renviron("mock_token", "MOCK_TOKEN_NAME"),
#                  "added")
#
#   expect_true(any(grepl("MOCK_TOKEN_NAME=mock_token",
#                         readLines(Renviron_path))))
#
#   expect_error(nettskjema_token2renviron("new_mock_token", "MOCK_TOKEN_NAME"),
#                "already exists")
#
#   # test overwrite
#   expect_message(nettskjema_token2renviron("new_mock_token", "MOCK_TOKEN_NAME",
#                                            action = "overwrite"),
#                  "forcing an overwrite")
#   expect_true(any(grepl("MOCK_TOKEN_NAME=new_mock_token",
#                         readLines(Renviron_path))))
#   expect_message(nettskjema_token2renviron("new_mock_token", "MOCK_TOKEN_NAME2",
#                                            action = "overwrite"),
#                  "adding new token")
#
#
#   # test delete
#   expect_message(nettskjema_token2renviron("new_mock_token", "MOCK_TOKEN_NAME",
#                             action = "delete"),
#                  "Deleting token name")
#   expect_message(nettskjema_token2renviron("new_mock_token", "MOCK_TOKEN_NAME2",
#                                            action = "delete"),
#                  "Deleting token name")
#   expect_false(any(grepl("MOCK_TOKEN_NAME=new_mock_token",
#                          readLines(Renviron_path))))
#
#   expect_error( nettskjema_token2renviron("new_mock_token", "MOCK_TOKEN_NAME",
#                                           action = "delete"),
#                 "does not exist.")
#
# })
#
# test_that("api_auth  works",{
#   skip_if_no_auth()
#   tt <- api_auth(test_token)
#   expect_length(strsplit(tt, "")[[1]],
#                 139)
# })
#
# test_that("nettskjema_api works",{
#   skip_if_no_auth()
#   resp <- nettskjema_api(paste0("forms/", test_form),
#                  test_token)
#
#   expect_length(resp, 10)
#   expect_equal(names(resp),
#                c("url", "status_code", "headers", "all_headers", "cookies",
#                  "content", "date", "times", "request", "handle"))
# })
#
# # These open something, how do I get that tested?
# test_that("nettskjema_user_create works",{
#   expect_error(nettskjema_user_create(), regexp = NA)
# })
#
# test_that("nettskjema_renviron_edit works",{
#   tmp <- expect_error(nettskjema_renviron_edit(), regexp = NA)
#
#   expect_equal(class(tmp), c("fs_path", "character"))
# })
#
