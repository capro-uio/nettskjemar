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

# assign b if a is nothing
`%||%` <- function(a, b) {
  if (length(a) == 0) return(b)
  if (is.na(a) | is.null(a) | a == "") return(b)
  a
}

merge_el <- function(df1, df2) {
  merge(df1, df2, by = "element_no", all = TRUE)
}

# Add variable labels
add_var_labels <- function(data, labels) {
  idx <- which(names(labels) %in% names(data))
  lapply(idx, function(x) {
    attr(data[, names(labels)[x]], "label") <<- unname(labels[x])
  })
  return(data)
}

# Add value labels
add_val_labels <- function(data, codebook) {
  cb_vars <- subset(codebook, !is.na(codebook$answer_code))
  variables <- stats::na.omit(unique(cb_vars$element_code))

  for (var in variables) {
    cb_var <- codebook[codebook$element_code %in% var, ]

    val_labs <- stats::setNames(
      utils::type.convert(
        cb_var$answer_code,
        tryLogical = FALSE,
        numerals = "no.loss",
        as.is = TRUE
      ),
      cb_var$answer_text
    )

    if (!is.null(data[[var]])) {
      data[[var]] <- structure(
        data[[var]],
        labels = val_labs,
        label = attr(data[[var]], "label"),
        class = c(
          "haven_labelled",
          "vctrs_vctr",
          class(val_labs)
        )
      )
    }
  }
  data
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

## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "question_codebook",
    "cb",
    "question",
    "answer",
    "columns",
    "string",
    "value",
    "form_id",
    "element_no",
    "submission_id"
  ))
}
