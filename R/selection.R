#' Makes a bit of javascript that lets us listen in on the selection
#'
#' @param editor_name  The name given the editor
#' @param position The starting row and column location
#'
#' @return javascript text
#' @export
#'
#' @examples
#'
make_js_listener <- function(editor_name, position){

  row_num <- position$row + 1
  col_num <- position$column + (nchar(new_text) - nchar(selected_text))

  script <- paste0("editor__",editor_name,".focus(); editor__",
                   editor_name,".gotoLine(", row_num, ",", col_num, ");")

  return(script)
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

  return(new_text)

}


replace_selection <- function(text, selection, replacement){

  text2 <- sub(pattern = selection, replacement = replacement, x = text, fixed = TRUE)
  return(text2)
}
