
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom purrr map_chr map
#' @importFrom tibble tibble
clean_form_submissions <- function(cont){

  dt <- tibble(
    submission_id = map_chr(cont, "submissionId")
  )

  bind_cols(
    dt,
    bind_rows(map(cont, extract_submission_answers))
  )
}

#' @importFrom purrr map
#' @importFrom tibble as_tibble
extract_submission_answers <- function(data, use_codebook = TRUE){

  tmp <- data$answers

  opt <- ifelse(use_codebook, "externalAnswerOptionId", "text" )

  qs <- map(tmp, "externalQuestionId")
  answ <- lapply(tmp, function(x) x[["answerOptions"]][[1]][[opt]])
  names(answ) <- qs

  as_tibble(lapply(answ, function(x) ifelse(is.null(x), NA, x)))
}
