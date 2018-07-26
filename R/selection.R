#' Makes a bit of javascript that lets us listen in on the selection
#'
#' @param editor_var 
#'
#' @return javascript text
#' @export
#'
#' @examples
#' 
make_js_listener <- function(editor_var){
  
  cmd <- paste0("editor__",editor_var, ".getSelection()")
  
  script <- paste0("document.onmouseup = document.onkeyup = document.onselectionchange = function() { var selection = ", cmd, ";",
                  "Shiny.onInputChange('sel', selection); };")
  
  return(script)
}
