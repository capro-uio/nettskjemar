# Extract element as user information
#' @importFrom purrr map_chr
#' @importFrom dplyr tibble
#' @noRd
as_user <- function(el){
  tibble(
    username = map_chr(el, "username", .default = NA),
    name = map_chr(el, "name", .default = NA)
  )
}

# Extract form elements list
#' @importFrom purrr map_chr map_int
#' @importFrom dplyr tibble
#' @noRd
as_element <- function(el){
  tmp <- tibble(
    type = map_chr(el, "elementType", .default = NA),
    order = map_int(el, "sequence", .default = NA)
  )

  # pre-allocate space to populate
  details <- element_details(tmp$type, el)

  tmp$details <- lapply(details, function(x) x)

  structure(tmp, class = c("nettskjema_elements", "list"))
}

#' @importFrom dplyr tibble
#' @noRd
as_img_element <- function(el){
  dt <- tibble(.rows = 1)

  dt$img_id = el[["image"]][["imageId"]]
  dt$img_url = el[["imageUrl"]]
  dt$img_text = el[["altText"]]
  dt$img_name = el[["image"]][["filename"]]

  dt
}

#' @noRd
as_txt_element <- function(el){
  strip_html(el$description)
}


as_pagebreak_element <- function(){
  NULL
}


#' @importFrom dplyr tibble
as_radio_element <- function(el){
  as_tibble(cbind(
    element_question(el),
    element_answeropts(el$questions[[1]])
  ))
}

#' @importFrom tidyr unnest
as_radiomatrix_element <- function(el){
  tmp <- element_matrix(el)
  tmp$answers <- lapply(1:nrow(tmp),
                        function(x) element_answeropts(el))
  unnest(tmp, answers)
}


#' @importFrom purrr map_chr map_lgl
#' @importFrom dplyr tibble
as_checkbox_element <- function(el, checkbox_type = c(string, list, columns)){
  cbind(
    tibble(
      max_selected = max_selected(el),
      question = strip_html(map_chr(el$questions, "text", .default = NA_character_)),
      question_mandatory = map_lgl(el$questions, "mandatory", .default = NA)),
    element_answeropts(el$questions[[1]])
  )
}

#' @importFrom dplyr tibble
#' @importFrom purrr map_chr map_lgl
as_checkboxmatrix_element <- function(el){
  tmp <- element_matrix(el)
  tmp$max_selected <- max_selected(el)
  tmp$answers <- lapply(1:nrow(tmp),
                        function(x) element_answeropts(el))

  unnest(tmp, answers)
}

#' @importFrom dplyr tibble
as_question_element <- function(el){
  tibble(element_question(el))
}



#' @importFrom dplyr tibble
as_select_element <- function(el){
  as_tibble(cbind(
    element_select(el),
    element_answeropts(el$questions[[1]])
  ))
}

as_linearscale_element <- function(el){
  element_linearscale(el)
}

element_linearscale <- function(el){
  cbind(
    element_main(el),
    tibble(
      question_order       = map_int(el$questions, "sequence", .default = NA_integer_),
      question_preselected = map_lgl(el$questions, "horizontal", .default = NA),
      question_rangemarks  = map_lgl(el$questions, "rangeMarksShown", .default = NA),
      question_scaletype   = el$linearScaleType,
      question_minvalue    = map_int(el$questions, "minimumValue", .default = NA_integer_),
      question_mintext     = map_chr(el$questions, "maximumValueText", .default = NA_character_),
      question_midtext     = map_chr(el$questions, "midValueText", .default = NA_character_),
      question_maxvalue    = map_int(el$questions, "maximumValue", .default = NA_integer_),
      question_maxtext     = map_chr(el$questions, "maximumValueText", .default = NA_character_)
    )
  )
}

element_main <- function(el){
  tibble(
    question           = map_chr(el$questions, "text", .default = NA_character_),
    question_descr     = strip_html(map_chr(el$questions, "description", .default = NA_character_)),
    question_codebook  = map_chr(el$questions, "externalQuestionId", .default = NA_character_),
    question_mandatory = map_lgl(el$questions, "mandatory", .default = NA)
  )
}

#' @importFrom purrr map_chr map_int map_lgl map
#' @importFrom dplyr bind_cols select one_of tibble
element_question <- function(el){
  element_main(el)
}

#' @importFrom purrr map_chr map_int map_lgl map
#' @importFrom dplyr bind_cols select one_of tibble
element_select <- function(el){
  cbind(
    element_main(el),
    question_order       = map_int(el$questions, "sequence", .default = NA_integer_),
    question_preselected = map_lgl(el$questions, "horizontal", .default = NA),
    question_rangemarks  = map_lgl(el$questions, "rangeMarksShown", .default = NA)
  )
}


#' @importFrom purrr map_chr
#' @importFrom dplyr tibble
element_checkbox <- function(el){
  element_main(el)
}


#' @importFrom purrr map_chr map_int map_lgl
#' @importFrom dplyr tibble
element_matrix <- function(el){
  cbind(
    element_main(el),
    question_order = map_int(el$questions, "sequence", .default = NA_integer_)
  )
}


#' @importFrom purrr map_chr map_lgl
#' @importFrom dplyr tibble
element_answeropts <- function(el){
  tibble(
    answer_order = map_chr(el$answerOptions, "sequence", .default = NA_character_),
    answer_option = map_chr(el$answerOptions, "text", .default = NA_character_),
    answer_codebook = map_chr(el$answerOptions, "externalAnswerOptionId", .default = NA_character_),
    answer_preselected = map_lgl(el$answerOptions, "preselected", .default = NA),
    answer_correct = map_lgl(el$answerOptions, "correct", .default = NA)
  )
}

element_details <- function(type, el){
  j <- sapply(type, function(x) list())
  j <- lapply(1:length(type), function(e){
    switch(type[e],
           "IMAGE"            = as_img_element(el[[e]]),
           "CHECKBOX"         = as_checkbox_element(el[[e]]),
           "MATRIX_CHECKBOX"  = as_checkboxmatrix_element(el[[e]]),
           "PAGE_BREAK"       = as_pagebreak_element(),
           "QUESTION"         = as_question_element(el[[e]]),
           "RADIO"            = as_radio_element(el[[e]]),
           "MATRIX_RADIO"     = as_radiomatrix_element(el[[e]]),
           "TEXT"             = as_txt_element(el[[e]]),
           "SELECT"           = as_select_element(el[[e]]),
           "LINEAR_SCALE"     = as_linearscale_element(el[[e]]),
           "unknown element class"
    )
  })
  unname(j)
}


element_exists <- function(el, fields){
  nms <- names(unlist(el))
  fields[fields %in% names(el)]
}


element_missing <- function(el, fields){
  nms <- names(unlist(el))
  fields[!fields %in% names(el)]
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("answers"))
}


