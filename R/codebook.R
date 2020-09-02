#' Grab codebook for a form
#'
#' Function works on meta-data downloaded from a form.
#' It will return a data.frame in long format
#'
#' @param meta_data form meta-data from \code{\link{nettskjema_get_meta}}
#'
#' @return long format tibble
#' @export
#'
#' @examples
#' \dontrun{
#' meta_110000 <- nettskjema_get_meta(110000)
#' codebook(meta_110000)
#' }
#' @importFrom dplyr bind_rows relocate
#' @importFrom readr type_convert cols
codebook <- function(meta_data){

  els <- meta_data$elements$details

  idx <- which(unlist(lapply(els, is.data.frame)))

  codes <- lapply(idx,
                  function(x) els[[x]])
  names(codes) <- meta_data$elements$order[idx]

  dt <- bind_rows(codes, .id = "element_no")
  dt <- type_convert(dt, col_types = cols())
  relocate(dt, element_no)
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
#' @param data nettskjema data
#' @param codebook codebook from same nettskjema
#' @param information character vector of information to add.
#' One or more of "order", "option", "correct" "preselected".
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
                            information = c("order", "option")){

  information <- match.arg(information,
                           c("order", "option",
                             "correct", "preselected"),
                           several.ok = TRUE)

  data_extra <- data[,0]

  questions <- unique(na.omit(cb$question_codebook))
  for(q in 1:length(questions)){
    col <- questions[q]

    tmp <- filter(codebook,
                  question_codebook == col)
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
    idx_cb <- grep("answer_codebook", names(tmp))
    dt_col[,2:5] <- tmp[idx, -idx_cb]
    dt_col <- dt_col[, -1]
    names(dt_col) <- gsub("tmp", col, names(dt_col))

    data_extra <- bind_cols(data_extra, dt_col)
  }

  data_extra <- select(data_extra,
                       matches(paste0(information, collapse="|", sep="$")))

  dt <- bind_cols(data, data_extra)
  dt <- dt[,order(colnames(dt))]
  relocate(dt, form_id)
}

#' @export
format.nettskjema_elements <- function(x, ...){
  browser()
  c(
    sprintf("# Nettskjema elements information for form %s", x$form_id),
    "",
    unname(sapply(c("title","language","opened", "respondents", "contact",
                    "codebook", "personal_data", "sensitive_data"),
                  function(i) sprintf("%s: %s", i, x[[i]]))
    ),

    sprintf("editors: %s", nrow(x$editors)),
    sprintf("no. elements: %s", length(x$elements$type))
  )
}

#' @export
print.nettskjema_elements <- function(x, ...){
  cat(format(x), sep="\n")
  invisible(x)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("question_codebook","cb",
                           "form_id", "element_no"))
}
