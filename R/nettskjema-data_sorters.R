
#' @importFrom dplyr bind_cols bind_rows mutate
#' @importFrom dplyr tibble across all_of
#' @importFrom purrr map_chr map
#' @importFrom tidyr pivot_longer pivot_wider separate_rows unite
#' @noRd
clean_form_submissions <- function(cont, cb, use_codebook,
                                   checkbox_type = c("string", "list", "columns"),
                                   checkbox_delim = ";"){
  checkbox_type <- match.arg(checkbox_type, c("string", "list", "columns"))

  dt <- tibble(
    submission_id = map_chr(cont, "submissionId")
  )

  dt <- bind_cols(
    dt,
    bind_rows(map(cont,
                  extract_submission_answers,
                  cb = cb,
                  use_codebook = use_codebook))
  )

  checkbox_idx <- which(apply(dt, 2, function(x) any(grepl(";-;", x))))

  if(checkbox_type == "string"){
    sub_it <- function(x){
      x <- strsplit(x, split = ";-;")
      x <- lapply(x, function(x) x[order(x)])
      x <- lapply(x, function(x) paste0(x, collapse = checkbox_delim))
      sapply(x, function(x) ifelse(x == "NA", NA, x))
    }
    mutate(dt, across(checkbox_idx, sub_it))
  }else if(checkbox_type == "list"){
    split_it <- function(x){
      x <- strsplit(x, split = ";-;")
      lapply(x, function(y) y[order(y)])
    }
    mutate(dt, across(checkbox_idx, split_it))
  }else if(checkbox_type == "columns"){
    dt <- pivot_longer(dt,
                       all_of(checkbox_idx),
                       names_to = "question",
                       values_to = "answer",
                       values_drop_na = TRUE)
    dt <- separate_rows(dt, answer, sep = ";-;")
    dt$value <- 1L
    dt <- unite(dt, question, question, answer)
    pivot_wider(dt,
                names_from = question,
                values_from = value,
                values_fill = 0L)
  }
}

#' @importFrom purrr map
#' @importFrom dplyr as_tibble
#' @noRd
extract_submission_answers <- function(cont, cb,
                                       use_codebook){
  type <- sapply(cont$answers, function(x) "answerOptions" %in% names(x) )

  opt <- ifelse(use_codebook,
                "externalAnswerOptionId",
                "text")

  answ <- lapply(1:length(cont$answers),
                 function(x){
                   if(type[x]){
                     paste0(map_chr(cont$answers[[x]][["answerOptions"]], opt),
                            collapse = ";-;")
                   }else{
                     cont$answers[[x]][["textAnswer"]]
                   }
                 })
  if(any(use_codebook & !inherits(cb, "data.frame"))){
    names(answ) <- map(cont$answers, "externalQuestionId")
  }else{
    nms <- select(cb, element_no, question)
    nms <- unique(nms)
    names(answ) <- nms$question
  }
  as_tibble(lapply(answ, function(x) ifelse(is.null(x), NA, x)),
            .name_repair = repair_names)
}

repair_names <- function(x){
  idx <- match(x, x)
  for(k in idx[duplicated(idx)]){
    i <- x[idx == k]
    x[idx == k] <- sprintf("%s_%02d", i, 1:length(i))
  }
  x
}


#' @importFrom httr content
#' @importFrom pbapply pblapply
#' @noRd
grab_data <- function(incremental, submissionIds, token_name, path, opts, ...) {
  if(incremental | length(submissionIds) > 2000){

    if(length(submissionIds) > 2000)
      cat("\tNumber of responses to download exceeds 2000, switching to incremental download.\n")

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

# Function to add additional columns to the data based on the codebook information
#' @importFrom dplyr filter select starts_with bind_cols matches relocate
#' @importFrom dplyr tibble rename_all
#' @noRd
get_extra_data <- function(questions, col, type, type_answ, data, information, cb) {

  # prep df for populating
  data_extra <- data[,0]

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

  for(inf in 1:length(information)){
    data_extra <- rename_all(data_extra, rn_cols,
                             from = information[inf],
                             to = names(information)[inf]
    )
  }

  bind_cols(data, data_extra)
}