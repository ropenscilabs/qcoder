#' Adds codes surrounding the selected text
#'
#' @param selection The selection of text to be coded
#' @param codes The code or codes to be added to the document

add_codes_to_selection <-  function(selection, codes) {

  code_vec <- paste0("#", codes)

  codes <- paste0(code_vec, collapse = ",")
  new_text <- paste0("(QCODE)", selection, "(/QCODE){", codes, "}")

  new_text

}


replace_selection <- function(text_search, selection, replacement){

  text2 <- sub(pattern = selection, replacement = replacement, x = text_search, fixed = TRUE)
  text2
}
