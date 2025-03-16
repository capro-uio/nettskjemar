test_that("ns_get_data retrieves data in original format", {
  vcr::use_cassette("ns_get_data_original", {
    result <- ns_get_data(form_id, type = "original")
    expect_true(is.data.frame(result)) # Check if the result is a data frame
    expect_gt(nrow(result), 0) # Check if rows exist in the data
  })
})

test_that("ns_get_data retrieves data in labelled format", {
  vcr::use_cassette("ns_get_data_labelled", {
    result <- ns_get_data(form_id, type = "labelled")
    expect_true(is.data.frame(result))
  })
})

test_that("ns_get_submission retrieves individual submission", {
  vcr::use_cassette("ns_get_submission", {
    result <- ns_get_submission(submission_id)
    expect_true(is.list(result)) # Check if the result is a list
    expect_gt(length(result), 0) # Ensure the result is not empty
  })
})
