#' remove file extension
#'
#' @template path
#'
#' @return string without file extension
#' @importFrom tools file_ext
#' @noRd
rm_ext <- function(path) {
  ex <- file_ext(path)
  ex <- sprintf(".%s$", ex)
  gsub(ex, "", path)
}

#' assign b if a is nothing
#' @noRd
`%||%` <- function(a, b) {
  if (length(a) == 0) return(b)
  if (is.na(a) || is.null(a) || a == "") return(b)
  a
}

merge_el <- function(df1, df2) {
  merge(df1, df2, by = "element_no", all = TRUE)
}


list2df <- function(x) {
  y <- lapply(x, list2row)
  y <- do.call(rbind, y)
  as.data.frame(y)
}

list2row <- function(x) {
  x <- null2na(x)
  as.data.frame(x)
}

null2na <- function(x) {
  idx <- which(sapply(x, is.null))
  for (i in idx) {
    x[[i]] <- NA
  }
  x
}
