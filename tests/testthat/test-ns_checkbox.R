# Mock data for testing
mock_data <- data.frame(
  formid = c(1, 2, 3),
  `$submission_id` = c("sub1", "sub2", "sub3"),
  "check.1.1" = c(1, 0, 1),
  "check.1.2" = c(0, 1, 0),
  "check.3.1" = c(0, 1, 0),
  "check.3.2" = c(1, 0, 1),
  check.names = FALSE
)

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

test_that("Returns expected subset of checkbox data", {
  result <- find_checkbox_matrix(mock_data, mock_codebook)
  expect_true(all(c("element_code", "lab_q", "lab_answ") %in% names(result)))
  expect_equal(nrow(result), 4)
})

test_that("Splits checkbox text correctly", {
  checkbox_text <- c("check.3.1", "check.3.2")
  result <- split_checkbox_matrix(
    checkbox_text,
    sep = "\\."
  )
  expect_equal(nrow(result), 2)
  # Check the third part of the split
  expect_equal(result[1, 3], "1")
  expect_equal(result[2, 3], "2")
})

# Test `checkbox2long`
test_that("Reshapes checkbox data into long format", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  result <- checkbox2long(mock_data, check_columns)
  expect_true(all(c("$submission_id", "value", "X2") %in% names(result)))
  # Should have 3 columns
  expect_equal(ncol(result), 3)
})

# Test `cbm_aggr`
test_that("Change checkbox data to list or character", {
  check_columns <- find_checkbox_matrix(mock_data, mock_codebook)
  long_data <- checkbox2long(mock_data, check_columns)
  result_list <- cbm_aggr(long_data, fun = list)
  expect_true(all(c("$submission_id", "check.1") %in% names(result_list)))

  # Test aggregation using paste
  result_char <- cbm_aggr(long_data, fun = paste, collapse = ",")
  # Check the output type
  expect_type(result_char, "list")
  expect_true(any(sapply(result_char[-1], is.character)))
})

test_that("transforms data correctly", {
  result <- ns_alter_checkbox(mock_data, to = "list", cb = mock_codebook)
  # Ensure transformed columns are removed
  expect_true(!"check.1.1" %in% names(result))
  # Check transformed column's name
  expect_true("check.1" %in% names(result))
})

test_that("Checks for MATRIX_CHECKBOX attributes", {
  test_vector <- 1:5
  attr(test_vector, "ns_type") <- "MATRIX_CHECKBOX"
  expect_true(is_checkbox_matrix(test_vector))

  test_vector_no_attr <- 1:5
  expect_false(is_checkbox_matrix(test_vector_no_attr))
})
