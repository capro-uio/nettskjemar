#' Add columns with the choice options rather than
#' their code
#'
#' The \code{add_code_string} adds columns to the data
#' with suffixes with the actual text answers provided
#' for each coded response.
#'
#' @param data nettskjema form data
#' @param codebook codebook from nettskjema
#' @param suffix suffix for columns with choice options
#' rather than code
#'
#' @return a [tibble][tibble::tibble()] type data.frame
#' @export
add_code_string <- function(data, codebook, suffix = "choice"){

  if(!is.data.frame(data)) {
    stop("The object supplied to `.data` is not a data.frame")
  }

  if(length(codebook) == 1){
    message("The object supplied to `codebook` looks like a path, attempting to read it in and clean")
    codebook <- codebook_read(codebook)
    codebook <- codebook_clean(codebook)
  }else if(!is.data.frame(codebook)) {
    message("The object supplied to `codebook` is not a data.frame, running `clean_codebook()`")
    codebook <- codebook_clean(codebook)
  }

  # Check if submissionId is coded as `NR` or `submissionId`
  submissionId <- c("submissionId", "NR")
  submissionId <- submissionId[submissionId %in% names(data)]
  submissionId <- dplyr::enquo(submissionId)

  # Check if any columns are for multiple-choice options
  # These columns have numbers suffixed to their names,
  # and require spcial handling

  find_mult <- function(find_in, col_names){
    n <- sapply(find_in, function(x) grepl(paste0(x,"_"), col_names))
    apply(n,2,any)
  }

  codebook <- codebook %>%
    dplyr::mutate(mult = find_mult(column, names(data))) %>%
    dplyr::group_by(column) %>%
    dplyr::mutate(n = cumsum(mult),
           n = ifelse(n == 0, NA, n)) %>%
    dplyr::mutate() %>%
    dplyr::ungroup(
      column = ifelse(is.na(n),
                      column,
                      paste(column,n, sep="_"))
    ) %>%
    dplyr::mutate(column, dplyr::select, option)

  data %>%
    tidyr::gather(column, code, -!!submissionId) %>%
    dplyr::left_join(codebook, by=c("column", "code")) %>%
    tidyr::gather(key, val, option) %>%
    dplyr::mutate(
      val = ifelse(is.na(val), code, val),
      key = ifelse(is.na(val), "", suffix)
    ) %>%
    tidyr::unite(key, c(column, key)) %>%
    dplyr::mutate(key = gsub("_$", "", key)) %>%
    dplyr::select(-code) %>%
    tidyr::spread(key, val)


}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("column", "n", "mult", "key", "val"))
}
