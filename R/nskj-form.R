#' Get data from a form
#'
#' This function connects to a specific form and fetches all answers.
#' The responses are placed in a data.frame for easy use.
#'
#' @template form_id
#' @template asis
#' @param labelled Logical. Should labelled data be 
#'   returned. Labelled data applies contextual
#'   information from the codebook into special
#'   attributes in the data.frame. See the 
#'   \code{labelled} package for more information.
#'
#' @return tibble data.frame
#' @export
#' @examples
#' \dontrun{
#'
#' # Retrieve all data
#' data_110000 <- nskj_get_data(110000)
#'
#' }
nskj_get_data <- function(form_id, asis = FALSE, labelled = !asis){
  if(asis){
    if(labelled)
      cli::cli_alert_warning("labels cannot be applied to raw data, ignoring {code labelled = TRUE}")
    
    resp <- nskj_req() |> 
      httr2::req_url_path_append("form", form_id, "answers") |> 
      httr2::req_perform(verbosity = 0)

    if(!httr2::resp_has_body(resp)){
      cli::cli_alert_info("Form has no answers. Returning nothing.")
      return(NULL)
    }

    cont <- httr2::resp_body_string(resp) 
    dt <- jsonlite::stream_in(
      textConnection(cont), 
      verbose = FALSE)
    return(dt)
  }

  dt_csv <- nskj_get_form_reports(
    form_id,
    type = "csv", 
    path = NULL) |> 
      httr2::resp_body_string()
  
  dt <- utils::read.csv2(
    textConnection(dt_csv), 
    check.names= FALSE)
  
  if(!labelled) return(dt)

  if(labelled && !has_codebook(form_id)  ){
    cli::cli_alert_warning("labels can only be used on data with a valid codebook, ignoring {code labelled = TRUE}")
    return(dt)
  }

  cb <- nskj_get_codebook(form_id)
  cols <- cb[, c("element_code", "element_text")] |> 
    unique() |> 
    stats::na.omit()
  cols <- setNames(cols$element_text, cols$element_code)

  if(length(cols) != ncol(dt)-3){
    cli::cli_abort("Number of columns in the data ({ncol(dt)-3}) does not match the codebook ({length(cols)}).")
  }

  dt <- add_var_labels(dt, cols)
  dt <- add_val_labels(dt, cb)

  return(dt)
}

#' Get all forms you have access to
#' 
#' All users have access to specific
#' forms, and this function uses the
#' token provided to check what the
#' current user has access to.
#'
#' @template asis
#' @return data.frame, if \code{asis = TRUE} returns a list
#' @export
nskj_get_forms <- function(asis = FALSE){
  resp <- nskj_req() |> 
    httr2::req_url_path_append("form", "me") |> 
    httr2::req_perform() |> 
    httr2::resp_body_json()

  if(asis) return(resp)

  list2df(resp)
}


#' Download files associated with a form
#' 
#' Each Nettskjema form has several 
#' auxiliary forms that can be retrieved.
#' 
#' @template form_id
#' @param type Character. Either "csv", "excel" or
#'   "spss".
#' @param path Character. Full path to where the 
#'   downloaded file should be saved. Defaults to
#'   current directory with correct extension.
#' @return httr2-response
#' @export
#' @examples
#' \dontrun{
#' nskj_get_form_reports(10009, type="csv")
#' nskj_get_form_reports(10009, type="excel")
#' nskj_get_form_reports(10009, type="spss")
#' nskj_get_form_reports(
#'   10009, 
#'   type="spss",
#'   path = "~/Desktop/10009/form.sav"
#' )
#' }
nskj_get_form_reports <- function(
  form_id,
  type = c("csv", "excel", "spss"), 
  path = report_path(form_id, type)){

  type <- match.arg(type)

  type <- switch(type,
    csv   = "csv-report",
    spss  = "spss-syntax",
    excel = "excel-report"
  )

  resp <- nskj_req() |> 
    httr2::req_url_path_append("form", form_id, type) |> 
    httr2::req_perform()
    
  if(!is.null(path))
    writeBin(httr2::resp_body_raw(resp), path)
  
  invisible(resp)
}

#' @noRd
report_path <- function(form_id, type){
  ext <- switch(type,
    csv   = "csv",
    spss  = "sav",
    excel = "xlsx"
  )
  sprintf("%s.%s", form_id, ext)
}
