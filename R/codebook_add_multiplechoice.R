#' Add indication if question allowed
#' multiple choice selection to codebook
#'
#' @param data nettskjema form data
#' @param codebook codebook from nettskjema
#'
#' @importFrom dplyr mutate group_by ungroup select
codebook_add_multiplechoice <- function(codebook, data){

  codebook %>%
    dplyr::mutate(mult = find_mult(column, names(data))) %>%
    dplyr::group_by(column) %>%
    dplyr::mutate(n = cumsum(mult),
           n = ifelse(n == 0, NA, n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      column = ifelse(is.na(n),
                      column,
                      paste(column,n, sep="_"))
    ) %>%
    dplyr::select(-n)

}

find_mult <- function(find_in, col_names){
  n <- sapply(find_in, function(x) grepl(paste0(x,"_"), col_names))
  apply(n,2,any)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("column", "n", "mult"))
}
