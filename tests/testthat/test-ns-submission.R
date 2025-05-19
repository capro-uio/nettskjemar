test_that("ns_get_data retrieves data in original format", {
  vcr::use_cassette("ns_get_data_original", {
    result <- ns_get_data(form_id, type = "original")
  })

  # Check if the result is a data frame
  expect_true(is.data.frame(result))
  # Check if rows exist in the data
  expect_gt(nrow(result), 0)
})

test_that("ns_get_submission retrieves individual submission", {
  vcr::use_cassette("ns_get_submission", {
    result <- ns_get_submission(submission_id)
  })

  # Check if the result is a list
  expect_true(is.list(result))
  # Ensure the result is not empty
  expect_gt(length(result), 0)
})
