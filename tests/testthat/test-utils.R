test_that("strip_html works", {
  expect_equal(strip_html("<p>"), "")
  expect_error(strip_html(""))
})

test_that("max_selected works", {
  mock_element <- list(maxSelectedAnswerOptions = 3)
  expect_equal(max_selected(mock_element), 3)
})
#
# test_that("response checks works", {
#   expect_false(is.response(1))
#
#   mock_resp <- structure(1, class = "response")
#   expect_true(is.response(mock_resp))
#
#   expect_false(is.app_json(1))
#   expect_false(is.app_json(mock_resp))
#
#   mock_resp <- structure(
#     list(
#       headers = list(
#         `content-type` = "application/json"
#         )
#       ),
#     class = "response")
#   # mock not made correctly
#   # expect_true(is.app_json(mock_resp))
# })

test_that("api_catch_error works", {
  expect_error(api_catch_error("something"),
               "did not return json")
})
