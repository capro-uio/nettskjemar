test_that("ns_get_attachment saves an attachment file", {
  vcr::use_cassette("ns_get_attachment", {
    temp_file <- tempfile(fileext = ".png")
    result <- ns_get_attachment(
      attachment_id,
      path = temp_file
    )
  })

  expect_s3_class(result, "httr2_response")
  unlink(temp_file) # Clean up
})

test_that("ns_list_form_attachments retrieves metadata", {
  vcr::use_cassette("ns_list_form_attachments", {
    result <- ns_list_form_attachments(form_id)
  })

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(all(
    c("submissionId", "answerAttachmentId", "filename") %in% colnames(result)
  ))
})

test_that("ns_get_form_attachments saves attachments", {
  vcr::use_cassette("ns_get_form_attachments", {
    output_dir <- tempfile()
    dir.create(output_dir)
    result <- ns_get_form_attachments(
      form_id,
      filenames = "standardized",
      output_dir = output_dir
    )
  })

  # Check if files are saved successfully
  expect_true(file.exists(output_dir))
  saved_files <- list.files(output_dir)
  expect_gt(length(saved_files), 0)
  unlink(output_dir, recursive = TRUE)
})

test_that("Retrieves submission attachment metadata", {
  vcr::use_cassette("ns_list_submission_attachments", {
    result <- ns_list_submission_attachments(submission_id)
  })

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true(all(
    c("counter", "filename", "answerAttachmentId") %in% colnames(result)
  ))
})

test_that("ns_get_submission_attachments saves  sub attch", {
  vcr::use_cassette("ns_get_submission_attachments", {
    output_dir <- tempfile()
    result <- ns_get_submission_attachments(
      submission_id,
      filenames = "original",
      output_dir = output_dir
    )
  })
  # Check if files are saved successfully
  expect_true(file.exists(output_dir))
  saved_files <- list.files(output_dir)
  expect_gt(length(saved_files), 0) # Ensure files were saved
  unlink(output_dir, recursive = TRUE) # Clean up
})
