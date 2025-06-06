test_that("fetches raw codebook correctly", {
  vcr::use_cassette("get_raw_codebook_valid", {
    with_mocked_nettskjema_auth(
      raw_cb <- get_raw_codebook(form_id)
    )
  })

  expect_s3_class(raw_cb, "ns_codebook_raw")
  expect_equal(raw_cb$form_id, form_id)
  expect_type(raw_cb$elements, "list")
})

vcr::use_cassette("get_raw_codebook_invalid", {
  test_that("handles invalid form_id gracefully", {
    with_mocked_nettskjema_auth(
      expect_error(get_raw_codebook(100), "Not Found")
    )
  })
})


test_that("converts raw to structured", {
  vcr::use_cassette("ns_get_codebook_valid", {
    with_mocked_nettskjema_auth(
      cb <- ns_get_codebook(form_id)
    )
  })

  expect_s3_class(cb, "ns_codebook")
  expect_true(ncol(cb) > 0)
})

test_that("respects asis flag", {
  vcr::use_cassette("ns_get_codebook_raw", {
    with_mocked_nettskjema_auth(
      cb_raw <- ns_get_codebook(
        form_id,
        asis = TRUE
      )
    )
  })

  expect_s3_class(cb_raw, "ns_codebook_raw")
})

test_that("creates long format tibble", {
  meta <- list(
    elements = list(
      list(
        elementType = "text",
        externalElementId = "e1",
        text = "Question 1",
        description = "Description 1",
        subElements = list(),
        answerOptions = list()
      ),
      list(
        elementType = "multiple_choice",
        externalElementId = "e2",
        text = "Question 2",
        description = "Description 2",
        subElements = list(),
        answerOptions = list(
          list(text = "Option 1", externalAnswerOptionId = "a1", sequence = 1),
          list(text = "Option 2", externalAnswerOptionId = "a2", sequence = 2)
        )
      )
    )
  )
  cb <- codebook(meta)
  expect_s3_class(cb, "ns_codebook")
  expect_true(nrow(cb) > 0)
  expect_true(ncol(cb) > 0)
})

test_that("returns formatted string", {
  raw_cb <- list(
    form_id = 123,
    elements = list(
      list(elementType = "text"),
      list(elementType = "multiple_choice")
    )
  )
  class(raw_cb) <- "ns_codebook_raw"
  formatted <- expect_output(
    format(raw_cb),
    "multiple_choice"
  )
  expect_type(formatted, "character")
  expect_contains(
    formatted,
    "# Nettskjema raw codebook for form 123"
  )
})

test_that("writes raw codebook to JSON", {
  temp_file <- tempfile(fileext = ".json")
  on.exit(unlink(temp_file))
  raw_cb <- list(
    form_id = 123,
    elements = list(list(elementType = "text"))
  )
  class(raw_cb) <- c("ns_codebook_raw", "list")
  ns_write_codebook(raw_cb, temp_file)
  expect_true(file.exists(temp_file))
  written_data <- jsonlite::fromJSON(temp_file)
  expect_equal(written_data$form_id, 123)
})

test_that("writes codebook to table", {
  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file))
  cb <- data.frame(
    element_no = 1,
    element_type = "text",
    element_code = "e1",
    element_text = "Question 1"
  )
  class(cb) <- c("ns_codebook", "data.frame")
  ns_write_codebook(cb, temp_file)
  expect_true(file.exists(temp_file))
  written_data <- read.table(
    temp_file,
    header = TRUE,
    sep = "\t"
  )
  expect_equal(nrow(written_data), 1)
  expect_equal(
    written_data$element_code[1],
    "e1"
  )
})

test_that("triggers warning", {
  expect_warning(
    ns_write_codebook(list(dummy = TRUE), "dummy.json"),
    "Cannot write object of class"
  )
})
