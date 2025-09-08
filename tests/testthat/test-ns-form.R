test_that("ns_get_forms returns data.frame by default", {
  vcr::use_cassette("ns_get_forms", {
    with_mocked_nettskjema_auth({
      formslist <- ns_get_forms()
    })
  })

  expect_is(formslist, "data.frame")
  expect_equal(nrow(formslist), 1)
  expect_equal(ncol(formslist), 14)
  expect_equal(
    names(formslist),
    c(
      "formId",
      "title",
      "openFrom",
      "openTo",
      "lastSubmissionDate",
      "modifiedDate",
      "personalDataErasedDate",
      "deliveryDestination",
      "anonymous",
      "numberOfDeliveredSubmissions",
      "owners",
      "isDictaphone",
      "myFormsFormListingGroup",
      "open"
    )
  )
  expect_is(formslist$isDictaphone, "logical")
})

test_that("ns_get_forms returns raw list when asis = TRUE", {
  vcr::use_cassette("ns_get_forms_raw", {
    with_mocked_nettskjema_auth({
      formslist_raw <- ns_get_forms(asis = TRUE)
    })
  })

  expect_is(formslist_raw, "list")
  expect_equal(length(formslist_raw), 1)
  expect_null(names(formslist_raw))
  expect_equal(
    names(formslist_raw[[1]]),
    c(
      "formId",
      "title",
      "openFrom",
      "openTo",
      "lastSubmissionDate",
      "modifiedDate",
      "personalDataErasedDate",
      "deliveryDestination",
      "anonymous",
      "numberOfDeliveredSubmissions",
      "owners",
      "isDictaphone",
      "myFormsFormListingGroup",
      "open"
    )
  )
})

# Tests for ns_get_form_reports function
test_that("ns_get_form_reports works with csv type", {
  tmpfile <- withr::local_tempfile(fileext = ".csv")
  vcr::use_cassette("ns_get_form_reports_csv", {
    with_mocked_nettskjema_auth({
      resp <- ns_get_form_reports(form_id, type = "csv", path = tmpfile)
    })
  })
  expect_s3_class(resp, "httr2_response")
  expect_true(file.exists(tmpfile))
})

test_that("ns_get_form_reports works with excel type", {
  tmpfile <- withr::local_tempfile(fileext = ".xlsx")
  vcr::use_cassette("ns_get_form_reports_excel", {
    with_mocked_nettskjema_auth({
      resp <- ns_get_form_reports(form_id, type = "excel", path = tmpfile)
    })
  })
  expect_s3_class(resp, "httr2_response")
  expect_true(file.exists(tmpfile))
})

test_that("ns_get_form_reports works with spss type", {
  tmpfile <- withr::local_tempfile(fileext = ".sav")
  vcr::use_cassette("ns_get_form_reports_sav", {
    with_mocked_nettskjema_auth({
      resp <- ns_get_form_reports(form_id, type = "spss", path = tmpfile)
    })
  })
  expect_s3_class(resp, "httr2_response")
  expect_true(file.exists(tmpfile))
})

test_that("ns_get_form_reports validates type argument", {
  expect_error(
    ns_get_form_reports(123823, type = "invalid"),
    "'arg' should be one of"
  )
})

# Tests for report_path helper function (even though it's internal)
test_that("report_path generates correct file extensions", {
  expect_equal(report_path(123, "csv"), "123.csv")
  expect_equal(report_path(456, "excel"), "456.xlsx")
  expect_equal(report_path(789, "spss"), "789.sav")
})

test_that("report_path handles different form_id types", {
  expect_equal(report_path("123", "csv"), "123.csv")
  expect_equal(report_path(123L, "csv"), "123.csv")
})
