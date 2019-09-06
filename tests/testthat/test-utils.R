test_that("strip_html works", {
  expect_equal(strip_html("<p>"), "")
  expect_error(strip_html(""))
})

test_that("max_selected works", {
  mock_element <- list(maxSelectedAnswerOptions = 3)
  expect_equal(max_selected(mock_element), 3)
})
