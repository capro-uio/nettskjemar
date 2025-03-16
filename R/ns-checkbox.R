#' Alter checkbox matrix variables in a dataset
#'
#' This function processes checkbox matrix variables in a dataset, transforming their representations into one of the specified formats.
#'
#' @param data A `data.frame` containing the dataset to modify.
#' @param to A character string specifying the output format for checkbox matrix variables. Must be one of "character" or "list".
#' @param sep A character string used as a separator when `to = "character"`. Defaults to `","`.
#' @param cb An optional codebook (`data.frame`) to identify checkbox matrix variables. If `NULL`, the codebook is generated using `ns_get_codebook()`.
#'
#' @return A modified `data.frame` with processed checkbox matrix variables.
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- ns_get_data(11000)
#' ns_alter_checkbox(data = dt, to = "list")
#' ns_alter_checkbox(data = dt, to = "character", sep = ";")
#' }
ns_alter_checkbox <- function(
  data,
  to = c("character", "list"),
  sep = ",",
  cb = NULL
) {
  to <- match.arg(to)

  check_cols <- find_checkbox_matrix(data, cb)

  dt <- switch(
    to,
    list = checkbox_list(data, check_cols),
    character = checkbox_character(data, check_cols, sep)
  )

  if (is.ns_data(data)) {
    for (i in 2:ncol(dt)) {
      cb_var <- subset(
        check_cols,
        check_cols$X2 == names(dt)[i]
      )
      dt[[i]] <- structure(
        dt[[i]],
        label = unique(cb_var$lab_q),
        ns_type = unique(cb_var$element_type),
        class = class(dt[[i]])
      )
    }
  }
  data <- data[, !names(data) %in% check_cols$element_code]
  merge(data, dt)
}

#' Identify Checkbox Matrix Variables in a Dataset
#'
#' This function identifies variables in a dataset that belong to the "MATRIX_CHECKBOX" type.
#' If no codebook is supplied, it generates one using `ns_get_codebook()`.
#'
#' @param data A `data.frame` containing the dataset.
#' @param cb An optional codebook (`data.frame`) to identify checkbox matrix variables. Defaults to `NULL`.
#'
#' @return A `data.frame` with additional columns:
#'   * `element_code`: The checkbox matrix variable codes.
#'   * `lab_full`: Full text label of the variable.
#'   * `lab_q`: Question text of the variable.
#'   * `lab_answ`: Answer text of the variable.
#'
#' @examples
#' # Example usage:
#' find_checkbox_matrix(data = dt, cb = cb)
#' find_checkbox_matrix(data = dt)
#'
#' @noRd
find_checkbox_matrix <- function(data, cb) {
  if (is.null(cb)) {
    cb <- ns_get_codebook(unique(data$formid))
  }
  cbm <- subset(cb, cb$element_type == "MATRIX_CHECKBOX")
  cbm_names <- cbm$element_code

  cbm_qs <- split_checkbox_matrix(cbm_names)
  names(cbm_qs)[1] <- "element_code"

  cbm_answ <- split_checkbox_matrix(
    cbm$element_text,
    " :: "
  )
  names(cbm_answ) <- c("lab_full", "lab_q", "lab_answ")

  merge(
    cbm,
    cbind(cbm_qs, cbm_answ)
  )
}

checkbox_list <- function(data, columns) {
  checkbox2long(data, columns) |>
    cbm_aggr(
      FUN = list
    )
}

checkbox_character <- function(data, columns, sep = ",") {
  checkbox2long(data, columns) |>
    cbm_aggr(
      FUN = paste,
      collapse = sep
    )
}

#' Aggregate Checkbox Matrix Variable Values
#'
#' This function aggregates checkbox matrix variable values using a specified function.
#'
#' @param data A `data.frame` in long format with checkbox matrix variables.
#' @param FUN The aggregation function to apply (e.g., `list`, `paste`).
#' @param ... Additional arguments passed to the aggregation function.
#'
#' @return A `data.frame` where each column represents an aggregated checkbox matrix variable.
#' @noRd
cbm_aggr <- function(data, FUN, ...) {
  aggr <- stats::aggregate(
    value ~ `$submission_id` + X2,
    data = data,
    FUN = FUN,
    ...
  )

  split_df <- split(aggr, aggr[["X2"]])
  split_df <- lapply(split_df, function(x) {
    names(x)[3] <- unique(x[["X2"]])
    x[, c(1, 3)]
  })

  dt <- split_df[[1]]
  for (k in 2:length(split_df)) {
    dt <- merge(dt, split_df[[k]], all = TRUE)
  }

  dt
}

#' Convert Checkbox Matrix Variables to Long Format
#'
#' This function reshapes checkbox matrix variables from wide format to long format.
#'
#' @param data A `data.frame` containing the dataset with checkbox matrix variables.
#' @param columns A `data.frame` specifying details of checkbox matrix variables, including their names and groupings.
#'
#' @return A `data.frame` in long format with columns:
#'   * `$submission_id`: Unique submission ID.
#'   * `value`: The selected checkbox option.
#'   * `X2`: Grouping variable indicating the checkbox matrix.
#' @noRd
checkbox2long <- function(data, columns) {
  ret_dat <- list()
  for (g in unique(columns$X2)) {
    cols <- subset(columns, columns$X2 %in% g)
    cols$time <- seq_along(cols$X2)

    dat <- data[, c("$submission_id", cols[["element_code"]])]
    dat <- as.matrix(dat) |>
      as.data.frame()

    # reshape data to long
    dat_long <- stats::reshape(
      dat,
      varying = cols[["element_code"]],
      v.names = "value",
      direction = "long"
    )

    dat_long <- merge(dat_long, cols)
    dat_long[["value"]] <- ifelse(
      dat_long[["value"]] == 1,
      dat_long[["X3"]],
      NA
    )

    dat_long <- subset(dat_long, !is.na(value))
    dat_long <- dat_long[, c("$submission_id", "value", "X2")]
    ret_dat <- c(
      ret_dat,
      list(dat_long)
    )
  }
  ret_dat <- do.call(rbind, ret_dat)
  stats::na.omit(ret_dat)
}

#' Identify Checkbox Matrix Variables
#'
#' This function checks whether a variable is a "MATRIX_CHECKBOX" type based on its attributes.
#'
#' @param x An object to check.
#'
#' @return A logical value: `TRUE` if the variable is a "MATRIX_CHECKBOX" type, otherwise `FALSE`.
#'
#' @noRd
is.checkbox_matrix <- function(x) {
  a <- attributes(x)$ns_type
  if (length(a) == 0) return(FALSE)

  attributes(x)$ns_type %in% "MATRIX_CHECKBOX"
}


#' Split the checkbox matrix elements
#'
#' This function splits elements of a checkbox matrix into their respective components
#' based on a specified separator.
#'
#' @param x A character vector containing the elements to split.
#' @param sep A character string specifying the separator to use for splitting. Defaults to `"\\."`.
#'
#' @return A `data.frame` with three columns:
#'   * The full element name.
#'   * The first two components of the element name joined by the separator.
#'   * The third component of the element name.
#'
#' @examples
#' # Example usage:
#' split_checkbox_matrix(c("form1.var1.opt1", "form2.var2.opt2"))
#' @noRd
split_checkbox_matrix <- function(x, sep = "\\.") {
  k <- lapply(strsplit(x, sep), function(y) {
    c(
      paste(y, collapse = "."),
      paste(y[1:2], collapse = "."),
      y[3]
    )
  })
  do.call(rbind, k) |>
    data.frame()
}
