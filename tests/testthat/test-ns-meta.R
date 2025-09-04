test_that("ns_get_meta for valid input", {
  vcr::use_cassette("ns_get_meta_valid", {
    with_mocked_nettskjema_auth(
      meta <- ns_get_meta(form_id)
    )
  })

  expect_s3_class(meta, "ns_meta")
  expect_equal(meta$form_id, form_id)
  expect_equal(meta$title, "API test form")
  expect_equal(
    meta$editorsContactEmail,
    "a.m.mowinckel@psykologi.uio.no"
  )
  expect_false(meta$isOpen)
  expect_true(meta$isCodebookValid)
  expect_equal(meta$numberOfSubmissions, 3)
})

test_that("ns_get_meta invalid form_id", {
  vcr::use_cassette("ns_get_meta_invalid", {
    with_mocked_nettskjema_auth(
      expect_error(
        ns_get_meta(100),
        "Not Found"
      )
    )
  })
})

test_that("meta_raw adds correct class", {
  content <- list(form_id = 123, title = "Test Form")
  meta <- meta_raw(content)
  expect_s3_class(meta, "ns_meta")
  expect_equal(meta$form_id, 123)
  expect_equal(meta$title, "Test Form")
})

test_that("format.ns_meta returns string", {
  meta <- meta_raw(list(
    form_id = 123,
    title = "Test Form",
    editorsContactEmail = "test@example.com",
    isOpen = TRUE,
    isCodebookValid = TRUE,
    numberOfSubmissions = 42,
    modifiedDate = "2023-10-01T12:00:00Z"
  ))
  formatted <- format(meta)
  expect_type(formatted, "character")
  expect_contains(
    formatted,
    "# Nettskjema raw metadata for form 123"
  )
  expect_contains(
    formatted,
    "title: Test Form"
  )
  expect_contains(
    formatted,
    "editorsContactEmail: test@example.com"
  )
  expect_contains(
    formatted,
    "isOpen: TRUE"
  )
})

test_that("writes data to a file", {
  temp_file <- withr::local_tempfile(fileext = ".json")
  meta <- meta_raw(list(
    form_id = 123,
    title = "Test Form",
    editorsContactEmail = "test@example.com",
    isOpen = TRUE,
    isCodebookValid = TRUE,
    numberOfSubmissions = 42,
    modifiedDate = "2023-10-01T12:00:00Z"
  ))
  ns_write_meta(meta, temp_file)
  # Check file written
  expect_true(file.exists(temp_file))
  # Validate file content
  written_data <- jsonlite::fromJSON(temp_file)
  expect_equal(written_data$form_id, 123)
  expect_equal(written_data$title, "Test Form")
})

test_that("changes non-json extension", {
  temp_file <- withr::local_tempfile(fileext = ".txt")
  meta <- meta_raw(list(
    form_id = 123,
    title = "Test Form"
  ))
  expect_message(
    ns_write_meta(
      meta,
      temp_file
    ),
    "Switching"
  )
  # Confirm JSON file exists
  expect_true(
    file.exists(
      paste0(rm_ext(temp_file), ".json")
    )
  )
})

test_that("ns_write_meta.default triggers warning", {
  expect_warning(
    ns_write_meta(
      list(dummy = TRUE),
      "dummy.json"
    ),
    "Cannot write object of"
  )
})
