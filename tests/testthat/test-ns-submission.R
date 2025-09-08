test_that("ns_get_submission returns JSON for a valid submission ID", {
  vcr::use_cassette("ns_get_submission_valid", {
    with_mocked_nettskjema_auth({
      result <- ns_get_submission(submission_id)
    })
  })
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("ns_get_submission handles invalid submission ID", {
  vcr::use_cassette("ns_get_submission_invalid", {
    with_mocked_nettskjema_auth({
      expect_error(
        ns_get_submission(999999),
        "404 Not Found"
      )
    })
  })
})

test_that("ns_get_submission handles edge cases gracefully", {
  with_mocked_nettskjema_auth({
    expect_error(ns_get_submission(NULL))

    expect_error(ns_get_submission("abc"))
  })
})

# Tests for ns_get_submission_pdf
test_that("ns_get_submission_pdf saves PDF to specified path", {
  temp_path <- withr::local_tempfile(fileext = ".pdf")

  vcr::use_cassette("ns_get_submission_pdf_valid", {
    with_mocked_nettskjema_auth({
      result <- ns_get_submission_pdf(27685292, temp_path)
    })
  })

  expect_null(result)
  expect_true(file.exists(temp_path))
  expect_gt(file.info(temp_path)$size, 0)
})

test_that("ns_get_submission_pdf uses default path when not specified", {
  default_path <- sprintf("%s.pdf", submission_id)
  on.exit(
    {
      if (file.exists(default_path)) {
        file.remove(default_path)
      }
    },
    add = TRUE
  )
  vcr::use_cassette("ns_get_submission_pdf_default_path", {
    with_mocked_nettskjema_auth({
      result <- ns_get_submission_pdf(submission_id)
    })
  })

  expect_null(result)
  expect_true(file.exists(default_path))
  expect_gt(file.info(default_path)$size, 0)
})

test_that("ns_get_submission_pdf handles invalid submission ID gracefully", {
  temp_path <- withr::local_tempfile(fileext = ".pdf")

  vcr::use_cassette("ns_get_submission_pdf_invalid", {
    with_mocked_nettskjema_auth({
      expect_error(
        ns_get_submission_pdf(999999, temp_path)
      )
    })
  })

  expect_false(file.exists(temp_path))
})

test_that("ns_get_submission_pdf handles edge cases gracefully", {
  temp_path <- withr::local_tempfile(fileext = ".pdf")
  expect_error(ns_get_submission_pdf(NULL, temp_path))

  expect_error(ns_get_submission_pdf("abc", temp_path))
})
