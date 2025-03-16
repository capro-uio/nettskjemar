test_that("ns_get_attachment saves an attachment file", {
  vcr::use_cassette("ns_get_attachment", {
    temp_file <- tempfile(fileext = ".png") # Change extension based on file type expected
    result <- ns_get_attachment(attachment_id, path = temp_file)
    expect_true(file.exists(temp_file)) # Ensure file is saved
    expect_s3_class(result, "httr2_response") # Check if the response is valid
    unlink(temp_file) # Clean up
  })
})

test_that("ns_list_form_attachments retrieves attachment metadata", {
  vcr::use_cassette("ns_list_form_attachments", {
    result <- ns_list_form_attachments(form_id)
    expect_true(is.data.frame(result)) # Check if the result is a data frame
    expect_gt(nrow(result), 0) # Ensure the data frame has rows
    expect_true(all(
      c("submissionId", "answerAttachmentId", "filename") %in% colnames(result)
    ))
  })
})

test_that("ns_get_form_attachments saves all form attachments", {
  vcr::use_cassette("ns_get_form_attachments", {
    output_dir <- tempfile() # Temporary directory for attachments
    dir.create(output_dir)
    result <- ns_get_form_attachments(
      form_id,
      filenames = "standardized",
      output_dir = output_dir
    )
    # Check if files are saved successfully
    expect_true(file.exists(output_dir))
    saved_files <- list.files(output_dir)
    expect_gt(length(saved_files), 0) # Ensure files were saved
    unlink(output_dir, recursive = TRUE) # Clean up
  })
})

test_that("ns_list_submission_attachments retrieves submission attachment metadata", {
  vcr::use_cassette("ns_list_submission_attachments", {
    result <- ns_list_submission_attachments(submission_id)
    expect_true(is.data.frame(result)) # Check if result is a data frame
    expect_gt(nrow(result), 0) # Ensure the data frame has rows
    expect_true(all(
      c("counter", "filename", "answerAttachmentId") %in% colnames(result)
    ))
  })
})

test_that("ns_get_submission_attachments saves all submission attachments", {
  vcr::use_cassette("ns_get_submission_attachments", {
    output_dir <- tempfile() # Temporary directory for attachments
    result <- ns_get_submission_attachments(
      submission_id,
      filenames = "original",
      output_dir = output_dir
    )
    # Check if files are saved successfully
    expect_true(file.exists(output_dir))
    saved_files <- list.files(output_dir)
    expect_gt(length(saved_files), 0) # Ensure files were saved
    unlink(output_dir, recursive = TRUE) # Clean up
  })
})
