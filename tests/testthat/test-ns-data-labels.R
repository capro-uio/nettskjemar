sample_data <- data.frame(
  var1 = c("1", "2", "3"),
  var2 = c("A", "B", "C"),
  var3 = c(NA, "Yes", "No")
)

sample_codebook <- data.frame(
  element_code = c("var1", "var1", "var1", "var2", "var3", "var3"),
  answer_code = c("1", "2", "3", NA, NA, NA),
  answer_text = c("Label1", "Label2", "Label3", NA, NA, "Yes"),
  element_text = c(
    "Variable 1",
    "Variable 1",
    "Variable 1",
    "Variable 2",
    "Variable 3",
    "Variable 3"
  ),
  element_type = c(
    "type_text",
    "type_text",
    "type_text",
    "type_character",
    "type_character",
    "type_character"
  )
)

test_that("Applies value labels based on codebook", {
  # Apply the function
  labelled_data <- ns_add_labels(
    sample_data,
    sample_codebook
  )

  # Check overall structure
  # Output should still be a data frame
  expect_true(is.data.frame(labelled_data))

  # Check if var1 is correctly converted
  # Check variable label
  expect_true(inherits(labelled_data$var1, "haven_labelled"))
  expect_equal(attr(labelled_data$var1, "label"), "Variable 1")
  # Check value labels
  expect_equal(
    attr(labelled_data$var1, "labels"),
    c(Label1 = 1, Label2 = 2, Label3 = 3)
  )

  # var2 should not have value labels,
  # as it's NA in the codebook
  expect_true(!inherits(labelled_data$var2, "haven_labelled"))
  # Check variable label
  expect_equal(attr(labelled_data$var2, "label"), "Variable 2")

  # var3 has no value labels due to only NA answer_code
  expect_true(!inherits(labelled_data$var3, "haven_labelled"))
  # Check variable label
  expect_equal(attr(labelled_data$var3, "label"), "Variable 3")

  attr(labelled_data$var3, "label") <- NULL
  attr(labelled_data$var3, "ns_type") <- NULL
  attr(labelled_data$var3, "class") <- NULL

  expect_equal(labelled_data$var3, sample_data$var3)
})

test_that("ns_add_labels handles missing values gracefully", {
  # Modify data to include more NAs
  data_with_na <- sample_data
  data_with_na$var1[3] <- NA

  # Apply the function
  labelled_data <- ns_add_labels(data_with_na, sample_codebook)

  # Check if missing values in var1 are preserved without issues
  expect_true(is.na(labelled_data$var1[3]))
})

test_that("dont modify non-codebook", {
  # Add a variable not in the codebook
  data_extra_var <- sample_data
  data_extra_var$var4 <- c(1, 2, 3)

  # Apply the function
  labelled_data <- ns_add_labels(data_extra_var, sample_codebook)

  # Check if var4 remains unmodified
  expect_null(attr(labelled_data$var4, "label"))
  expect_null(attr(labelled_data$var4, "labels"))
})

test_that("ns_add_labels works with an empty dataset", {
  # Apply the function on an empty dataset
  empty_data <- data.frame()
  result <- ns_add_labels(empty_data, sample_codebook)

  # Result should also be an empty data frame
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 0)
})
