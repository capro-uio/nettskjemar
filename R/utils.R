#' @param codebook codebook from nettskjema
#' @export
codebook_read <- function(codebook_path, ...){
  readLines(con = codebook_path, ...)
}

#' @importFrom stringr str_split str_sub
#' @importFrom stringr str_split str_sub
#' @param codebook codebook from nettskjema
#' @export
codebook_title <- function(codebook){
  x <- unlist(stringr::str_split(codebook[1], " \\(id "))[1]
  as.character(stringr::str_sub(x, 3))
}

#' @importFrom stringr str_split
#' @importFrom stringr str_split
#' @param codebook codebook from nettskjema
#' @export
codebook_id <- function(codebook){
  x <- unlist(stringr::str_split(codebook[1], " \\(id "))[2]
  x <- unlist(stringr::str_split(x, "\\) "))[1]
  as.integer(x)
}

#' @importFrom dplyr as_tibble
#' @importFrom utils read.table
#' @export
form_read <- function(nettskjema_path,
                            sep = "\t", header = TRUE,
                            dec = ",", stringsAsFactors = FALSE,
                            ...){
  x <- utils::read.table(nettskjema_path,
             sep = sep, header = header,
             dec = dec, stringsAsFactors = stringsAsFactors,
             ...)
  dplyr::as_tibble(x)
}
