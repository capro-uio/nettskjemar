test_that("test get forms list", {
  vcr::use_cassette("ns_get_forms", {
    with_mocked_nettskjema_auth(
      formslist <- ns_get_forms()
    )
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


test_that("test get raw forms list", {
  vcr::use_cassette("ns_get_forms_raw", {
    with_mocked_nettskjema_auth(
      formslist_raw <- ns_get_forms(asis = TRUE)
    )
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
