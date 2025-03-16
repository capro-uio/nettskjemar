#' Add value labels
#' @noRd
add_labels <- function(data, codebook) {
  cb <- subset(codebook, codebook$element_code %in% names(data))

  for (var in cb$element_code) {
    cb_var <- codebook[codebook$element_code %in% var, ]
    cb_vals <- subset(cb_var, !is.na(cb_var$answer_code))

    if (nrow(cb_vals) == 0) {
      vals <- utils::type.convert(
        data[[var]],
        tryLogical = FALSE,
        numerals = "no.loss",
        as.is = TRUE
      )

      data[[var]] <- structure(
        data[[var]],
        label = unique(cb_var$element_text),
        ns_type = unique(cb_var$element_type),
        class = class(vals)
      )
    } else {
      val_labs <- stats::setNames(
        utils::type.convert(
          cb_var$answer_code,
          tryLogical = FALSE,
          numerals = "no.loss",
          as.is = TRUE
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

is.ns_data <- function(x) {
  inherits(x, "ns-data")
}
