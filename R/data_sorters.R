
clean_form_submissions <- function(cont){

  dt <- tibble::tibble(
    submission_id = purrr::map_chr(cont, "submissionId")
  )

  dplyr::bind_cols(
    dt,
    dplyr::bind_rows(purrr::map(cont, extract_submission_answers))
  )
}

extract_submission_answers <- function(data, use_codebook = TRUE){

  tmp <- data$answers

  opt <- ifelse(use_codebook, "externalAnswerOptionId", "text" )

  qs <- purrr::map(tmp, "externalQuestionId")
  answ <- lapply(tmp, function(x) x[["answerOptions"]][[1]][[opt]])
  names(answ) <- qs

  tibble::as_tibble(lapply(answ, function(x) ifelse(is.null(x), NA, x)))
}
