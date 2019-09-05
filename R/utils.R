

strip_html <- function(s) {
  gsub("\\n", "", rvest::html_text(xml2::read_html(s)))
}

max_selected <- function(x){
  t <- x$maxSelectedAnswerOptions
  ifelse(t == 0, NaN, t)
}
