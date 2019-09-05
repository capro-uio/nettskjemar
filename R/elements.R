as_user <- function(element){
  tibble::tibble(
    username = purrr::map_chr(element, "username"),
    name = purrr::map_chr(element, "name")
  )
}

as_element <- function(element){
  tmp <- tibble::tibble(
    type = purrr::map_chr(element, "elementType"),
    order = purrr::map_int(element, "sequence")
  )

  # pre-allocate space to populate
  details <- element_details(tmp$type, element)

  tmp$details <- lapply(details, function(x) x)
  tmp
}

as_img_element <- function(img_element){
  tibble::tibble(
    img_id = img_element$image$imageId,
    img_name = img_element$image$filename
  )
}

as_txt_element <- function(txt_element){
  tibble::tibble(
    txt_desc = strip_html(purrr::map_chr(txt_element, "description")),
  )
}

as_pagebreak_element <- function(){
  NA
}

as_radio_element <- function(radio_element){
  tibble::as_tibble(cbind(
    element_question(radio_element),
    element_answeropts(radio_element$questions[[1]])
  ))
}

as_radiomatrix_element <- function(){
  element_answeropts(radio_m_element)
}

as_checkbox_element <- function(cb_element){
  cbind(
    tibble::tibble(
      max_selected = max_selected(cb_element),
      question = strip_html(purrr::map_chr(cb_element$questions, "description")),
      question_mandatory = purrr::map_lgl(cb_element$questions, "mandatory")),
    element_answeropts(cb_element$questions[[1]])
  )
}

as_checkboxmatrix_element <- function(){

}

as_question_element <- function(q_element){
  tibble::as_tibble(element_question(q_element))
}

as_select_element <- function(){

}

element_question <- function(el){
  # when visibility is toggled, data looks different
  if("answerOptionsVisibilityFilter" %in% names(el)){

  }else{

  }
  tibble::tibble(
    question = strip_html(purrr::map_chr(el$questions, "description")),
    question_codebook = purrr::map_chr(el$questions, "externalQuestionId"),
    question_mandatory = purrr::map_lgl(el$questions, "mandatory")
  )
}

element_answeropts <- function(el){
  tibble::tibble(
    answer_order = purrr::map_chr(el$answerOptions, "sequence"),
    answer_option = purrr::map_chr(el$answerOptions, "text"),
    answer_codebook = purrr::map_chr(el$answerOptions, "externalAnswerOptionId"),
    answer_preselected = purrr::map_lgl(el$answerOptions, "preselected"),
    answer_correct = purrr::map_lgl(el$answerOptions, "correct")
  )
}

element_checkbox <- function(el){
  tibble::tibble(
    order = purrr::map_chr(el, "sequence"),
    text = strip_html(purrr::map_chr(el, "description")),
    mandatory = purrr::map_chr(el, "mandatory")
  )
}

element_details <- function(type, element){
  j <- sapply(type, function(x) list())
  for(el in 1:length(type)){
    j[[el]] <- switch(type[el],
                      "IMAGE"         = as_img_element(element[[el]]),
                      "CHECKBOX"      = as_checkbox_element(element[[el]]),
                      "PAGE_BREAK"    = as_pagebreak_element(),
                      # "QUESTION"      = as_question_element(element[[el]]),
                      # "RADIO"         = as_radio_element(element[[el]]),
                      "unknown element class"
    )
  }

  unname(j)
}
