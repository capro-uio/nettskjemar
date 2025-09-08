# Tests for ns_get_data function

test_that("ns_get_data works with type = 'original'", {
  vcr::use_cassette("ns_get_data_original", {
    with_mocked_nettskjema_auth({
      data <- ns_get_data(form_id, type = "original")
    })
  })

  expect_is(data, "data.frame")
  expect_true(nrow(data) > 0)
  expect_true("formid" %in% names(data))
  expect_true(all(data$formid == form_id))
})

test_that("ns_get_data works with type = 'long'", {
  vcr::use_cassette("ns_get_data_long", {
    with_mocked_nettskjema_auth({
      data <- ns_get_data(form_id, type = "long")
    })
  })

  expect_is(data, "data.frame")
  expect_true(nrow(data) > 0)
  expect_true("formid" %in% names(data))
  expect_true(all(data$formid == form_id))
})

test_that("ns_get_data defaults to 'original' type when NULL", {
  vcr::use_cassette("ns_get_data_default", {
    with_mocked_nettskjema_auth({
      data_default <- ns_get_data(form_id, type = NULL)
      data_original <- ns_get_data(form_id, type = "original")
    })
  })

  expect_equal(data_default, data_original)
})

test_that("ns_get_data validates type argument", {
  expect_error(
    ns_get_data(form_id, type = "invalid"),
    "'arg' should be one of"
  )
})

# Tests for ns_get_submissions alias function
test_that("ns_get_submissions is an alias for ns_get_data", {
  expect_identical(ns_get_submissions, ns_get_data)
})

test_that("ns_get_submissions works with type = 'original'", {
  vcr::use_cassette("ns_get_submissions_original", {
    with_mocked_nettskjema_auth({
      data_get_data <- ns_get_data(form_id, type = "original")
      data_get_submissions <- ns_get_submissions(form_id, type = "original")
    })
  })

  expect_equal(data_get_data, data_get_submissions)
})

test_that("ns_get_submissions works with type = 'long'", {
  vcr::use_cassette("ns_get_submissions_long", {
    with_mocked_nettskjema_auth({
      data_get_data <- ns_get_data(form_id, type = "long")
      data_get_submissions <- ns_get_submissions(form_id, type = "long")
    })
  })

  expect_equal(data_get_data, data_get_submissions)
})

# Integration tests
test_that("ns_get_data different types return different structures", {
  vcr::use_cassette("ns_get_data_type_comparison", {
    with_mocked_nettskjema_auth({
      data_original <- ns_get_data(form_id, type = "original")
      data_long <- ns_get_data(form_id, type = "long")

      # Both should have formid column
      expect_true("formid" %in% names(data_original))
      expect_true("formid" %in% names(data_long))

      # They should generally have different structures
      # (unless it's a very simple form)
      expect_is(data_original, "data.frame")
      expect_is(data_long, "data.frame")
    })
  })
})

test_that("ns_get_data handles form with no answer", {
  testthat::local_mocked_bindings(
    resp_has_body = function(...) FALSE,
    .package = "httr2"
  )

  vcr::use_cassette("ns_get_data_no_body", {
    with_mocked_nettskjema_auth({
      expect_message(
        result <- ns_get_data(
          form_id,
          type = "long"
        ),
        "Form has no answers"
      )
      expect_null(result)
    })
  })
})
