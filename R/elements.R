#' Extract element as user information
#'
#' @template el
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
as_user <- function(el){
  tibble(
    username = map_chr(el, "username", .default = NA),
    name = map_chr(el, "name", .default = NA)
  )
}

#' Extract form elements list
#'
#' @template el
#' @importFrom purrr map_chr map_int
#' @importFrom tibble tibble
as_element <- function(el){
  tmp <- tibble(
    type = map_chr(el, "elementType", .default = NA),
    order = map_int(el, "sequence", .default = NA)
  )

  # pre-allocate space to populate
  details <- element_details(tmp$type, el)

  tmp$details <- lapply(details, function(x) x)

  structure(tmp, class = "nettskjema_elements")
}

#' Title
#'
#' @template el
#' @importFrom tibble tibble
as_img_element <- function(el){
  dt <- tibble(.rows = 1)

  dt$img_id = el[["image"]][["imageId"]]
  dt$img_url = el[["imageUrl"]]
  dt$img_text = el[["altText"]]
  dt$img_name = el[["image"]][["filename"]]

  dt
}

#' Title
#'
#' @template el
as_txt_element <- function(el){
  strip_html(el$description)
}

#' Title
as_pagebreak_element <- function(){
  NULL
}

#' Title
#'
#' @template el
#' @importFrom tibble tibble
as_radio_element <- function(el){
  as_tibble(cbind(
    element_question(el),
    element_answeropts(el$questions[[1]])
  ))
}

#' Title
#'
#' @template el
#' @importFrom tidyr unnest
as_radiomatrix_element <- function(el){
  tmp <- element_matrix(el)
  tmp$answers <- lapply(1:nrow(tmp),
                        function(x) element_answeropts(el))

  unnest(tmp, answers)
}

#' Title
#'
#' @template el
#' @importFrom purrr map_chr map_lgl
#' @importFrom tibble tibble
as_checkbox_element <- function(el){
  cbind(
    tibble(
      max_selected = max_selected(el),
      question = strip_html(map_chr(el$questions, "description")),
      question_mandatory = map_lgl(el$questions, "mandatory")),
    element_answeropts(el$questions[[1]])
  )
}

#' Title
#'
#' @template el
as_checkboxmatrix_element <- function(el){

}

#' Title
#'
#' @template el
#' @importFrom tibble as_tibble
as_question_element <- function(el){
  as_tibble(element_question(el))
}

#' Extract select element
#'
#' @template el
#' @importFrom tibble tibble
as_select_element <- function(el){
  as_tibble(cbind(
    element_question(el),
    element_answeropts(el$questions[[1]])
  ))
}

#' Extract question element
#'
#' @template el
#' @importFrom purrr map_chr map_int map_lgl map
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols select one_of
element_question <- function(el){
  colnms <- c(question = "description",
              question_codebook = "externalQuestionId",
              question_mandatory = "mandatory")

  exsts <- element_exists(el$questions[[1]],
                          colnms)
  tmp <- sapply(exsts,
                function(x) map(el$questions, x)
  )

  for(tt in element_missing(el$questions[[1]],
                            colnms)){
    tmp[[names(colnms)[colnms == tt]]] <- "<p>"
  }

  tmp <- bind_cols(tmp)
  tmp$question <- strip_html(tmp$question)

  select(tmp, one_of(names(colnms)))

  # This should work when each field is returned, even empty ones
  # tibble(
  #   question = strip_html(map_chr(el$questions, "description", .default = NA)),
  #   question_codebook = map_chr(el$questions, "externalQuestionId", .default = NA),
  #   question_mandatory = map_lgl(el$questions, "mandatory", .default = NA)
  # )
}

#' Extract answer options
#'
#' @template el
#' @importFrom purrr map_chr map_lgl
#' @importFrom tibble tibble
element_answeropts <- function(el){
  tibble(
    answer_order = map_chr(el$answerOptions, "sequence", .default = NA),
    answer_option = map_chr(el$answerOptions, "text", .default = NA),
    answer_codebook = map_chr(el$answerOptions, "externalAnswerOptionId", .default = NA),
    answer_preselected = map_lgl(el$answerOptions, "preselected", .default = NA),
    answer_correct = map_lgl(el$answerOptions, "correct", .default = NA)
  )
}

#' Extract checkbox element
#'
#' @template el
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
element_checkbox <- function(el){
  tibble(
    order = map_chr(el, "sequence", .default = NA),
    text = strip_html(map_chr(el, "description", .default = NA)),
    mandatory = map_chr(el, "mandatory", .default = NA)
  )
}

#' Extract matrix element
#'
#' @template el
#' @importFrom purrr map_chr map_int map_lgl
#' @importFrom tibble tibble
element_matrix <- function(el){
  # This should work when each field is returned, even empty ones
  tibble(
    question = map_chr(el$questions, "text", .default = NA),
    question_order = map_int(el$questions, "sequence", .default = NA),
    question_codebook = map_chr(el$questions, "externalQuestionId", .default = NA),
    question_mandatory = map_lgl(el$questions, "mandatory", .default = NA)
  )
}

#' Grabs information about nettskjema elements
#'
#' @param type type of element
#' @template el
element_details <- function(type, el){
  j <- sapply(type, function(x) list())
  for(e in 1:length(type)){
    j[[e]] <- switch(type[e],
                      "IMAGE"         = as_img_element(el[[e]]),
                      "CHECKBOX"      = as_checkbox_element(el[[e]]),
                      "PAGE_BREAK"    = as_pagebreak_element(),
                      "QUESTION"      = as_question_element(el[[e]]),
                      "RADIO"         = as_radio_element(el[[e]]),
                      "MATRIX_RADIO"  = as_radiomatrix_element(el[[e]]),
                      "TEXT"          = as_txt_element(el[[e]]),
                      "SELECT"        = as_select_element(el[[e]]),
                      "unknown element class"
    )
  }

  unname(j)
}

#' Title
#'
#' @template el
#' @template fields
element_exists <- function(el, fields){
  nms <- names(unlist(el))
  fields[fields %in% names(el)]
}

#' Title
#'
#' @template el
#' @template fields
element_missing <- function(el, fields){
  nms <- names(unlist(el))
  fields[!fields %in% names(el)]
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("answers"))
}


