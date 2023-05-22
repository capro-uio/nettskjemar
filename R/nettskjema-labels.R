
#' @importFrom cli cli_warn
nettskjema_label_values <- function(data,
                                    fct_levels = "order",
                                    fct_labels = "codebook",
                                    metadata = NULL){
  fct_labels <- match.arg(fct_labels,
                          c("option", "codebook"))
  fct_levels <- match.arg(fct_levels,
                          c("order", "codebook"))

  if(is.null(metadata))
    metadata <- nettskjema_get_meta(unique(data$form_id))
  cb <- codebook(metadata, unique(data$form_id))
  if(any(is.na(cb$question_codebook))){
    cli_warn("Not all values have codebook entries, labelling does not work in this context. Returning unlabelled data.")
    return(data)
  }

  fct_idx <- grep("RADIO|^CHECKBOX$",
                  metadata$elements$type)
  cb <- dplyr::filter(cb, element_no %in% fct_idx)
  cb <- dplyr::arrange(cb, element_no, !!fct_levels)
  cb$answer_order <- as.integer(cb$answer_order)
  cb <- as.data.frame(cb, stringsAsFactors = FALSE)

  dt <- as.data.frame(data, stringsAsFactors = FALSE)
  for(i in fct_idx){
    cb_tmp <- dplyr::filter(cb, element_no == i)
    levs <- cb_tmp[["answer_codebook"]]
    labs <- cb_tmp[[sprintf("answer_%s", fct_labels)]]
    col <- unique(cb_tmp$question_codebook)
    vec <- dplyr::pull(dt, !!unique(cb_tmp$question_codebook))
    vec <- factor(vec,
                  levels = levs,
                  labels = labs)
    vals <- cb_tmp[, c("answer_order", "answer_codebook", "answer_option")]
    names(vals) <- c("order", "code", "answer")
    attributes(vec)$labels <- vals
    attributes(vec)$class <- c("lbl", "factor")
    dt[, col] <- vec
  }
  dplyr::as_tibble(dt)
}


#' Add variable labels
#'
#' The Nettskjema forms come with meta-data that
#' allows for nicer handling of label information.
#' This function will use the metadata and and apply
#' labels to the variables.
#'
#' @template data
#' @template checkbox_type
#' @param metadata Optional, raw metadata as returned from Nettskjema API.
#' @importFrom cli cli_warn
#' @return tibble with label attributed to columns
#' @export
#'
# #' @examples
nettskjema_label_vars <- function(data, checkbox_type = "string", metadata = NULL){
  if(is.null(metadata))
    metadata <- nettskjema_get_meta(unique(data$form_id))
  labs <- codebook(metadata, unique(data$form_id))
  if(any(is.na(labs$question_codebook))){
    cli_warn("Not all variables have codebook entries, labelling does not work in this context. Returning unlabelled variables.")
    return(data)
  }

  lbsord <- data.frame(
    element_type = metadata$elements$type,
    element_no = as.character(metadata$elements$order),
    stringsAsFactors = FALSE)
  labs <- dplyr::left_join(labs, lbsord, by ="element_no")

  varlabs <- labs[,c("element_type",
                     "question",
                     "question_codebook",
                     "answer_codebook")]
  if(checkbox_type == "columns"){
    varlabs <- dplyr::mutate(
      varlabs,
      question_codebook = dplyr::case_when(
        grepl("CHECKBOX",element_type) ~ paste(
          question_codebook,
          answer_codebook,
          sep = "_"),
        TRUE ~ question_codebook
      ))
  }
  varlabs$element_type <- NULL
  varlabs$answer_codebook <- NULL
  varlabs <- unique(varlabs)
  varlabs <- stats::setNames(varlabs$question,
                             varlabs$question_codebook)
  varlabs <- c("form_id" = "Nettskjema form id", "submission_id" = "Unique response ID", varlabs)

  varlabs <- varlabs[names(varlabs) %in% names(data)]
  labelled::set_variable_labels(data, !!!varlabs)
}


# custom lbl format for factors ----
#' @importFrom utils capture.output
format.lbl <- function(x, ...) {
  labs <- attributes(x)$labels
  c(
    capture.output(factor(x)),
    "Labels:",
    paste(names(labs), collapse = "\t"),
    apply(labs, 1, paste, collapse = "\t")
  )
}

#' @export
print.lbl <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

