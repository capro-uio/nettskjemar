#' Make a data.frame of a nettskjema codebook
#'
#' \code{codebook_clean} wrangles the codebook into a tall data.frame
#'
#' @author Athanasia Mowinckel
#'
#' @importFrom dplyr filter mutate lag select transmute full_join as_tibble
#' @importFrom stringr str_split
#' @importFrom tidyr fill
#' @importFrom magrittr "%>%"
#'
#' @param codebook codebook from nettskjema
#'
#' @return a [tibble][tibble::tibble()] type data.frame
#'
#' @export
codebook_clean <- function(codebook){

  if(length(codebook) == 1){
    message("The object supplied to `codebook` looks like a path, attempting to read it in and clean")
    codebook <- codebook_read(codebook)
    stopifnot(length(codebook)>1)
  }

  clean <- codebook[-c(1:5)]

  x <- stringr::str_split(clean, "\t") %>%
    lapply(function(t) if(length(t) == 2) t else rep(t,2))

  # Initiate a data.frame
  clean <- data.frame(code = fix_var(x,1),
                      value = fix_var(x,2),
                      stringsAsFactors = FALSE)

  # Identify questions
  clean <- clean %>%
    dplyr::mutate(
      cc = ifelse((is.na(dplyr::lag(code, 2)) & is.na(dplyr::lag(code))), 1, 0),
      question = ifelse(cc == 1, value, NA),
      question = ifelse(question == "null", code, question)
    )

  # Identify choice options
  clean <- clean %>%
    dplyr::mutate(
      ct = ifelse(dplyr::lag(cc,2) == 1, 1, 0 ),
      ct = ifelse(dplyr::lag(ct,2) == 1, 1, ct ),
      ct = ifelse(ct == 1 & is.na(code), 0, ct),
      option = ifelse(ct == 1, value, NA),
      code2 = ifelse(ct == 1, code, NA)
    )

  # Small cleanup of temp cols and pure NA rows
  clean <- clean %>%
    dplyr::select(-cc, -ct) %>%
    rm_na_rows()

  # Fill in question column with last carried forward
  clean <- clean %>%
    tidyr::fill(question)

  # Split things up
  ## Only options
  opts <- clean %>%
    dplyr::filter(!is.na(option)) %>%
    dplyr::transmute(question = question,
              option = option,
              code = code2)

  ## Only questions
  qqs <- clean %>%
    dplyr::filter(dplyr::lag(question) != question) %>%
    dplyr::transmute(question = question,
              column = code)

  qqs %>%
    dplyr::full_join(opts, by="question",) %>%
    dplyr::as_tibble()

}


## Some necessary small fixer functions, internal
fix_var <- function(x, col){
  x <- unlist(lapply(x, function(x) x[col]))
  ifelse(x == "", NA, x)
}

rm_na_rows <- function(x){
  logic <- apply(x,1, function(t) !all(is.na(t)))
  x[logic,]
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("code", "cc", "code2", "ct", "option", "question", "value"))
}

