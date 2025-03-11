formid <- 413658

test_that("test get data", {
  vcr::use_cassette("get_data", {
    form_dt_raw <- nskj_get_data(formid, asis = TRUE)
    form_dt_lab <- nskj_get_data(formid)
    form_dt_unl <- nskj_get_data(formid, labelled = FALSE)
  })

  # raw data
  expect_is(form_dt_raw, "data.frame")
  expect_equal(nrow(form_dt_raw), 924)
  expect_equal(ncol(form_dt_raw), 17)
  expect_equal(
    names(form_dt_raw),
    c(
      "formId",
      "submissionId",
      "answerId",
      "elementId",
      "externalElementId",
      "textAnswer",
      "answerOptionIds",
      "externalAnswerOptionIds",
      "elementType",
      "createdDate",
      "modifiedDate",
      "subElementId",
      "answerAttachmentId",
      "filename",
      "mediaType",
      "size",
      "attachment"
    )
  )

  # labelled data
  expect_is(form_dt_lab, "data.frame")
  expect_equal(nrow(form_dt_lab), 39)
  expect_equal(ncol(form_dt_lab), 58)
  expect_equal(
    names(form_dt_lab)[c(1:2, 58)],
    c("$submission_id", "$created", "$answer_time_ms")
  )
  expect_match(
    attr(form_dt_lab$date, "label"),
    "What day is this about?"
  )

  expect_match(
    attr(form_dt_lab$activity_mental.worked, "label"),
    "Which mental activities did you do today\\?: Worked"
  )
  expect_is(
    form_dt_lab$activity_mental.worked,
    "haven_labelled"
  )

  # unlabelled data
  expect_is(form_dt_unl, "data.frame")
  expect_equal(nrow(form_dt_unl), 39)
  expect_equal(ncol(form_dt_unl), 58)
  expect_equal(
    names(form_dt_unl)[c(1:2, 58)],
    c("$submission_id", "$created", "$answer_time_ms")
  )
  expect_null(
    attr(form_dt_unl$date, "label")
  )
  expect_null(
    attr(form_dt_unl$activity_mental.worked, "label")
  )
})

test_that("test get forms list", {
  vcr::use_cassette("get_forms", {
    formslist <- nskj_get_forms()
    formslist_raw <- nskj_get_forms(asis = TRUE)
  })

  expect_is(formslist, "data.frame")
  expect_equal(nrow(formslist), 191)
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

  # raw
  expect_is(formslist_raw, "list")
  expect_equal(length(formslist), 14)
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
  expect_is(formslist$isDictaphone, "logical")
})
