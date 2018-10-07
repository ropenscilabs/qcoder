#' Makes a bit of javascript that lets us listen in on the selection
#'
#' @param editor_name  The name given the editor
#' @param position The starting row and column location
#' @param new_text  The new text including the code markup
#' @param selected_text The text to be marked with a code or codes
#'
#' @return javascript text
#' @export
#'
#' @examples
#'
make_js_listener <- function(editor_name, position, new_text, selected_text){

  row_num <- position$row + 1
  col_num <- position$column + (nchar(new_text) - nchar(selected_text))

  script <- paste0("editor__",editor_name,".focus(); editor__",
                   editor_name,".gotoLine(", row_num, ",", col_num, ");")

  script
}

#' Adds codes surrounding the selected text
#'
#' @param selection The selection of text to be coded
#' @param codes The code or codes to be added to the document
#'
#' @return
#'
#' @examples
add_codes_to_selection <-  function(selection, codes) {

  code_vec <- paste0("#", codes)

  codes <- paste0(code_vec, collapse = ",")
  new_text <- paste0("(QCODE)", selection, "(/QCODE){", codes, "}")

  new_text

}


replace_selection <- function(text, selection, replacement){

  text2 <- sub(pattern = selection, replacement = replacement, x = text, fixed = TRUE)
  text2
}
