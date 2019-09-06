# # Set test params
# test_token <- "NETTSKJEMA_TEST_TOKEN"
# test_form <- 123823
#
# test_that("nettskjema_get_data works", {
#   skip_if_no_auth()
#   test_dt <- nettskjema_get_data(test_form, token_name = test_token)
#
#   expect_equal(class(test_dt), c("tbl_df", "tbl", "data.frame"))
# })
#
# # test_that("nettskjema_get_meta works",{
# #
# #   meta <- nettskjema_get_meta(form_id = test_form,
# #                               token_name = test_token)
# #
# # })
