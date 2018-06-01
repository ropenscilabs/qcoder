# This creates a standard set of folders for a QCoder project
#' @param project_name  A string project name to be located in the
#'                      current working directory or a path to a project folder.
#' @example
#' create_qcoder_project("my_qcoder_project")
#' @export
create_qcoder_project<- function(project_name){
  dir.create(project_name)
  dir.create(paste0(project_name, "/documents"))
  dir.create(paste0(project_name, "/codes"))
  dir.create(paste0(project_name, "/data_frames"))
  dir.create(paste0(project_name, "/units"))
  invisible(TRUE)
}

#' This launches the data-reader Shiny app
#'
#' @example
#' read_data()
#' @export
read_data <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/data-reader"))
}

#' This launches the coder Shiny app
#'
#' @example
#' read_data()
#' @export
qcode_edit <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/qcoder-edit"))
}
