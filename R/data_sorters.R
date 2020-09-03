
#' @importFrom dplyr bind_cols bind_rows relocate
#' @importFrom purrr map_chr map
#' @importFrom tibble tibble
clean_form_submissions <- function(cont, cb, use_codebook = TRUE){

  dt <- tibble(
    submission_id = map_chr(cont, "submissionId")
  )

  bind_cols(
    dt,
    bind_rows(map(cont,
                  extract_submission_answers,
                  cb = cb,
                  use_codebook = use_codebook))
  )
}

#' @importFrom purrr map
#' @importFrom tibble as_tibble
extract_submission_answers <- function(cont, cb, use_codebook = TRUE){
  opt <- ifelse(use_codebook, "externalAnswerOptionId", "text" )

  tmp <- cont$answers

  qs <- map(tmp, "externalQuestionId")
  answ <- map(tmp, function(x)
    paste0(map_chr(x[["answerOptions"]], opt), collapse = ";"))
  names(answ) <- qs

  as_tibble(lapply(answ, function(x) ifelse(is.null(x), NA, x)))
}

#' @importFrom httr content
#' @importFrom pbapply pblapply
grab_data <- function(incremental, submissionIds, token_name, path, opts, ...) {
  if(incremental | length(submissionIds) > 2000){

    if(length(submissionIds) > 2000)
      cat("Number of responses to download exceeds 1000, switching to incremental download.\n")

    submissionIds <- file.path("submissions", submissionIds)

    resp <- pblapply(
      submissionIds,
      function(x) nettskjema_api(x,
                                 token_name = token_name, ...)
    )

    j <- lapply(resp, api_catch_error)

    cont <- lapply(resp, content)
  }else{

    resp <- nettskjema_api(paste0(path, opts),
                           token_name = token_name, ...)

    api_catch_error(resp)

    cont <- content(resp)

  }
  return(cont)
}



#' Add additional answer data
#'
#' The answers in the nettskjema forms
#' have some extra information attached
#' that is not returned by default.
#' This is for instance information on
#' the order of the answer options, the true
#' text, whether the answer is correct or
#' if it is preselected. This function makes
#' it possible to retrieve and add this
#' information to the nettskjema data, using
#' the codebook.
#'
#' @template data
#' @param information character vector of information to add.
#' One or more of "order", "option", "correct" "preselected".
#' @param codebook codebook object retreived by \code{\link{nettskjema_get_codebook}}
#' @template use_codebook
#' @template token_name
#' @param ... arguments passed to \code{\link[httr]{GET}}
#'
#' @return tibble with added columns
#' @export
#'
# #' @examples
#' @importFrom dplyr filter select starts_with bind_cols matches relocate
#' @importFrom stats na.omit
#' @importFrom tibble tibble
add_answer_data <- function(data,
                            codebook,
                            information = c("order", "option"),
                            use_codebook = TRUE,
                            token_name = "NETTSKJEMA_API_TOKEN",
                            ...){

  information <- match.arg(information,
                           c("order", "option",
                             "correct", "preselected"),
                           several.ok = TRUE)

  type <- ifelse(use_codebook, "question_codebook", "question")
  type_answ <- ifelse(use_codebook, "answer_codebook", "answer_option")

  # reduce codebook to only those with answer options
  cb <- codebook[!is.na(codebook[,type_answ]),]

  # Those without order are check-boxes
  cb <- cb[!is.na(cb[,"question_order"]),]

  # prep dataframe for populating
  data_extra <- data[,0]

  # get unique questions
  questions <- unname(unlist(unique(cb[,type])))

  # If they have set up questions that are not represented in the actual
  # question part of the form, this process will fail to merge properly.
  if(any(is.na(questions)))
    stop(paste("The codebook is not set up correctly, or some questions do not have text.",
               "Adding extra information from the codebook is not possible in this situation.",
               "Try filling out the codebook, before downloading the data once more.", sep="\n"),
         call. = FALSE)

  for(q in 1:length(questions)){
    col <- questions[q]

    tmp <- cb[cb[,type] == col,]
    tmp <- tmp[!is.na(tmp[,type]),]
    tmp <- select(tmp,
                  starts_with("answer"))
    dt_col <- tibble(
      tmp = unlist(data[, col]),
      tmp_order = NA,
      tmp_option = NA,
      tmp_preselected = NA,
      tmp_correct = NA
    )

    idx <- match(dt_col$tmp, tmp$answer_codebook)
    idx_cb <- grep(type_answ, names(tmp))
    dt_col[,2:5] <- tmp[idx, -idx_cb]
    dt_col <- dt_col[, -1]
    names(dt_col) <- gsub("tmp", col, names(dt_col))

    data_extra <- bind_cols(data_extra, dt_col)
  }

  data_extra <- select(data_extra,
                       matches(paste0(information, collapse="|", sep="$")))

  bind_cols(data, data_extra)
}
