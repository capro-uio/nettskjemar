#' Add Labels to Dataset
#'
#' The `ns_add_labels` function applies
#'  labels to variables in a dataset
#' based on a provided codebook. It
#'  assigns variable labels, value
#' labels,
#' and metadata required for the
#' "ns-data" class.
#'
#' @param data data from \code{ns_get_data}
#' @param codebook object from
#'     \code{ns_get_codebook}
#' @return A data frame with the same
#'   structure as `data`, but with
#'   variable and value labels applied.
#'   The resulting object is assigned
#'   the class `"ns-data"`.
#'
#' @examples
#' \dontrun{
#' data <- ns_get_data(11000)
#' cb <- ns_get_codebook(11000)
#' labeled_data <- ns_add_labels(data, cb)
#' }
#'
#' @export
ns_add_labels <- function(data, codebook) {
  cb <- subset(codebook, codebook$element_code %in% names(data))

  for (var in cb$element_code) {
    cb_var <- codebook[codebook$element_code %in% var, ]
    cb_vals <- subset(cb_var, !is.na(cb_var$answer_code))

    if (nrow(cb_vals) == 0) {
      vals <- type_convert(
        data[[var]]
      )

      data[[var]] <- structure(
        data[[var]],
        label = unique(cb_var$element_text),
        ns_type = unique(cb_var$element_type),
        class = class(vals)
      )
    } else {
      val_labs <- stats::setNames(
        type_convert(
          cb_var$answer_code
        ),
        cb_var$answer_text
      )

      data[[var]] <- structure(
        data[[var]],
        labels = val_labs,
        label = unique(cb_var$element_text),
        ns_type = unique(cb_var$element_type),
        class = c(
          "haven_labelled",
          "vctrs_vctr",
          class(val_labs)
        )
      )
    }
  }
  structure(
    data,
    class = c("ns-data", class(data))
  )
}

is_ns_data <- function(x) {
  inherits(x, "ns-data")
}

type_convert <- function(x) {
  utils::type.convert(
    x,
    na.strings = c("", "NA", " ", "NaN"),
    tryLogical = FALSE,
    numerals = "no.loss",
    as.is = TRUE
  )
}
