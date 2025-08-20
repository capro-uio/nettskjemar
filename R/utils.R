#' remove file extension
#'
#' @template path
#'
#' @return string without file extension
#' @importFrom tools file_ext
#' @noRd
rm_ext <- function(path) {
  ex <- file_ext(path)
  if (!nzchar(ex)) {
    return(path)
  }
  ex <- sprintf(".%s$", ex)
  gsub(ex, "", path)
}

#' assign b if a is nothing
#' @noRd
`%||%` <- function(a, b) {
  if (length(a) == 0) {
    return(b)
  }
  if (is.na(a) || is.null(a) || a == "") {
    return(b)
  }
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

mock_if_no_auth <- function() {
  if (!nettskjemar::ns_has_auth()) {
    Sys.setenv(NETTSKJEMA_CLIENT_ID = "a1b2c3d4-e5f6-7890-abcd-ef1234567890")
    Sys.setenv(
      NETTSKJEMA_CLIENT_SECRET = "aB3xK9mP2vQ8fR7nL98Mcs81sT4uY6wE5zC0hJ9iO3kM8pN2qA7bD1gF4jH6lS9vX3nR5mT8"
    )
  }
}
