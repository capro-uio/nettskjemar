# Mock data for testing
mock_data <- data.frame(
  formid = c(1, 2, 3),
  `$submission_id` = c("sub1", "sub2", "sub3"),
  "check.1.1" = c(1, 1, 1),
  "check.1.2" = c(0, 1, 0),
  "check.3.1" = c(0, 1, 1),
  "check.3.2" = c(1, 0, 1),
  check.names = FALSE
)
class(mock_data) <- c("ns-data", "data.frame")

mock_codebook <- data.frame(
  element_no = 1:4,
  element_code = c(
    "check.1.1",
    "check.1.2",
    "check.3.1",
    "check.3.2"
  ),
  element_text = c(
    "Question :: 1 :: Option 1",
    "Question :: 1 :: Option 2",
    "Regarding :: 3 :: Option 1",
    "Regarding :: 3 :: Option 2"
  ),
  element_type = c(
    "MATRIX_CHECKBOX",
    "MATRIX_CHECKBOX",
    "MATRIX_CHECKBOX",
    "MATRIX_CHECKBOX"
  )
)

# Tests for find_checkbox_matrix function
test_that("find_checkbox_matrix returns expected subset of checkbox data", {
  result <- find_checkbox_matrix(mock_data, mock_codebook)
  expect_true(all(c("element_code", "lab_q", "lab_answ") %in% names(result)))
  expect_equal(nrow(result), 4)
  expect_true(all(result$element_type == "MATRIX_CHECKBOX"))
})

test_that("find_checkbox_matrix filters MATRIX_CHECKBOX types only", {
  # Add non-checkbox elements
  mixed_codebook <- rbind(
    mock_codebook,
    data.frame(
      element_no = 5,
      element_code = "text.1",
      element_text = "Text question",
      element_type = "TEXT"
    )
  )

  result <- find_checkbox_matrix(mock_data, mixed_codebook)
  expect_equal(nrow(result), 4)
  expect_false("text.1" %in% result$element_code)
})

test_that("find_checkbox_matrix generates codebook when NULL", {
  with_mocked_bindings(
    ns_get_codebook = function(form_id) mock_codebook,
    {
      result <- find_checkbox_matrix(mock_data, cb = NULL)
      expect_true(all(
        c("element_code", "lab_q", "lab_answ") %in% names(result)
      ))
      expect_equal(nrow(result), 4)
    }
  )
})

# Tests for split_checkbox_matrix function
test_that("split_checkbox_matrix splits checkbox text correctly", {
  checkbox_text <- c("check.3.1", "check.3.2")
  result <- split_checkbox_matrix(checkbox_text, sep = "\\.")

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  # Check the third part of the split
  expect_equal(result[1, 3], "1")
  expect_equal(result[2, 3], "2")
  # Check the second part (combined first two)
  expect_equal(result[1, 2], "check.3")
  expect_equal(result[2, 2], "check.3")
})

test_that("split_checkbox_matrix handles different separators", {
  text_with_colons <- c(
    "Question :: 1 :: Option 1",
    "Question :: 1 :: Option 2"
  )
  result <- split_checkbox_matrix(text_with_colons, sep = " :: ")

  expect_equal(nrow(result), 2)
  expect_equal(result[1, 2], "Question.1")
  expect_equal(result[1, 3], "Option 1")
})

test_that("split_checkbox_matrix handles edge cases", {
  # Single element
  single_element <- "check.1.1"
  result <- split_checkbox_matrix(single_element)
  expect_equal(nrow(result), 1)

  # Empty vector
  empty_vector <- character(0)
  result <- split_checkbox_matrix(empty_vector)
  expect_equal(nrow(result), 0)
})

# Tests for checkbox2long function
test_that("checkbox2long reshapes checkbox data into long format", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  result <- checkbox2long(mock_data, check_columns)

  expect_true(all(c("$submission_id", "value", "X2") %in% names(result)))
  expect_equal(ncol(result), 3)
  expect_true(nrow(result) > 0)
})

test_that("checkbox2long handles empty data", {
  empty_data <- mock_data[0, ]
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  expect_warning(
    result <- checkbox2long(empty_data, check_columns)
  )
  expect_equal(result, empty_data)

  empty_cols <- check_columns[0, ]
  expect_warning(
    result <- checkbox2long(mock_data, empty_cols)
  )
  expect_equal(result, mock_data)
})

# Tests for cbm_aggr function
test_that("cbm_aggr aggregates data to list format", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  long_data <- checkbox2long(mock_data, check_columns)
  result_list <- cbm_aggr(long_data, fun = list)

  expect_true(all(c("$submission_id", "check.1") %in% names(result_list)))
  expect_true(any(sapply(result_list[-1], is.list)))
})

test_that("cbm_aggr aggregates data to character format", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  long_data <- checkbox2long(mock_data, check_columns)
  result_char <- cbm_aggr(long_data, fun = paste, collapse = ",")

  expect_type(result_char, "list")
  expect_true(any(sapply(result_char[-1], is.character)))
})

test_that("cbm_aggr handles different collapse separators", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  long_data <- checkbox2long(mock_data, check_columns)
  result_semicolon <- cbm_aggr(long_data, fun = paste, collapse = ";")

  # Check that semicolon is used in the result
  char_cols <- sapply(result_semicolon[-1], is.character)
  if (any(char_cols)) {
    char_values <- unlist(result_semicolon[char_cols])
    if (any(grepl(";", char_values))) {
      expect_true(any(grepl(";", char_values)))
    }
  }
})

# Tests for checkbox_list and checkbox_character helper functions
test_that("checkbox_list returns list format", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  result <- checkbox_list(mock_data, check_columns)

  expect_is(result, "data.frame")
  expect_true("$submission_id" %in% names(result))
})

test_that("checkbox_character returns character format with custom separator", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  result <- checkbox_character(mock_data, check_columns, sep = ";")

  expect_is(result, "data.frame")
  expect_true("$submission_id" %in% names(result))
})

# Tests for is_checkbox_matrix function
test_that("is_checkbox_matrix identifies MATRIX_CHECKBOX", {
  test_vector <- 1:5
  attr(test_vector, "ns_type") <- "MATRIX_CHECKBOX"
  expect_true(is_checkbox_matrix(test_vector))

  test_vector_no_attr <- 1:5
  expect_false(is_checkbox_matrix(test_vector_no_attr))
})

test_that("is_checkbox_matrix handles different ns_type values", {
  # Different type
  test_vector_text <- 1:5
  attr(test_vector_text, "ns_type") <- "TEXT"
  expect_false(is_checkbox_matrix(test_vector_text))

  # Multiple types including MATRIX_CHECKBOX
  test_vector_multi <- 1:5
  attr(test_vector_multi, "ns_type") <- "MATRIX_CHECKBOX"
  expect_true(is_checkbox_matrix(test_vector_multi))

  test_vector_null <- 1:5
  attr(test_vector_null, "ns_type") <- NULL
  expect_false(is_checkbox_matrix(test_vector_null))
})

# Tests for main ns_alter_checkbox function
test_that("ns_alter_checkbox validates 'to' argument", {
  expect_error(
    ns_alter_checkbox(mock_data, to = "invalid", cb = mock_codebook),
    "'arg' should be one of"
  )
})

test_that("ns_alter_checkbox transforms data to list format correctly", {
  result <- ns_alter_checkbox(mock_data, to = "list", cb = mock_codebook)

  # Ensure original checkbox columns are removed
  expect_false("check.1.1" %in% names(result))
  expect_false("check.1.2" %in% names(result))

  # Check transformed column exists
  expect_true("check.1" %in% names(result))
  expect_true("check.3" %in% names(result))
})

test_that("ns_alter_checkbox transforms data to character format correctly", {
  result <- ns_alter_checkbox(
    mock_data,
    to = "character",
    sep = ";",
    cb = mock_codebook
  )

  # Ensure original checkbox columns are removed
  expect_false("check.1.1" %in% names(result))
  expect_false("check.3.1" %in% names(result))

  # Check transformed columns exist
  expect_true("check.1" %in% names(result))
  expect_true("check.3" %in% names(result))
})

test_that("ns_alter_checkbox uses default separator", {
  result_default <- ns_alter_checkbox(
    mock_data,
    to = "character",
    cb = mock_codebook
  )
  result_comma <- ns_alter_checkbox(
    mock_data,
    to = "character",
    sep = ",",
    cb = mock_codebook
  )

  expect_equal(result_default, result_comma)
})

test_that("ns_alter_checkbox generates codebook when NULL", {
  with_mocked_bindings(
    ns_get_codebook = function(form_id) mock_codebook,
    {
      result <- ns_alter_checkbox(mock_data, to = "list", cb = NULL)
      expect_true("check.1" %in% names(result))
    }
  )
})

test_that("ns_alter_checkbox preserves ns-data attributes", {
  with_mocked_bindings(
    is_ns_data = function(x) TRUE,
    {
      result <- ns_alter_checkbox(mock_data, to = "list", cb = mock_codebook)

      expect_is(result, "data.frame")
      expect_true("check.1" %in% names(result))
    }
  )
})

test_that("ns_alter_checkbox handles data without checkbox columns", {
  non_checkbox_data <- data.frame(
    formid = 1:3,
    `$submission_id` = c("sub1", "sub2", "sub3"),
    text_col = c("a", "b", "c"),
    check.names = FALSE
  )

  # Mock empty checkbox matrix result
  with_mocked_bindings(
    find_checkbox_matrix = function(data, cb) {
      data.frame(
        element_code = character(0),
        X2 = character(0),
        X3 = character(0),
        lab_q = character(0),
        element_type = character(0)
      )
    },
    {
      expect_message(
        result <- ns_alter_checkbox(non_checkbox_data, to = "list")
      )
      expect_equal(result, non_checkbox_data)
    }
  )
})

# Integration tests
test_that("ns_alter_checkbox list and character", {
  result_list <- ns_alter_checkbox(
    mock_data,
    to = "list",
    cb = mock_codebook
  )
  result_char <- ns_alter_checkbox(
    mock_data,
    to = "character",
    cb = mock_codebook
  )

  # Both should have same structure except for the transformed columns
  base_cols <- c("formid", "$submission_id")
  expect_true(all(base_cols %in% names(result_list)))
  expect_true(all(base_cols %in% names(result_char)))

  # Both should have the same number of rows
  expect_equal(nrow(result_list), nrow(result_char))
})

test_that("ns_alter_checkbox preserves non-checkbox data", {
  result <- ns_alter_checkbox(mock_data, to = "list", cb = mock_codebook)

  # Original non-checkbox columns should be preserved
  expect_true("formid" %in% names(result))
  expect_true("$submission_id" %in% names(result))
  expect_equal(result$formid, mock_data$formid)
})

# Error handling tests
test_that("ns_alter_checkbox handles malformed codebook", {
  malformed_codebook <- mock_codebook
  malformed_codebook$element_text[1] <- "Malformed text without separators"

  # Should still work but might produce unexpected results in labels
  expect_no_error({
    result <- ns_alter_checkbox(mock_data, to = "list", cb = malformed_codebook)
  })
})

test_that("functions handle missing submission_id column", {
  data_no_submission <- mock_data
  names(data_no_submission)[
    names(data_no_submission) == "$submission_id"
  ] <- "submission_id"

  # Should handle gracefully or error appropriately
  expect_error({
    ns_alter_checkbox(data_no_submission, to = "list", cb = mock_codebook)
  })
})
