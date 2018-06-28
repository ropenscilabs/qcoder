#' Create a standard set of folders for a QCoder project
#'
#' @param project_name  A string project name to be located in the
#'                      current working directory or a path to a project folder.
#' @param sample Logical that indicates that the sample data should be copied to the project.
#' @examples create_qcoder_project(project_name = "my_qcoder_project")
#' @export
create_qcoder_project<- function(project_name, sample = FALSE){
  dir.create(project_name)
  dir.create(paste0(project_name, "/documents"))
  dir.create(paste0(project_name, "/codes"))
  dir.create(paste0(project_name, "/data_frames"))
  dir.create(paste0(project_name, "/units"))
  dir.create(paste0(project_name, "/images"))
  dir.create(paste0(project_name, "/media"))
  dir.create(paste0(project_name, "/memos"))
  dir.create(paste0(project_name, "/misc"))
  if (sample){
    examples <- list.files(system.file("Example_Data_Markedup",  package = "qcoder"))
    examples <- paste0(system.file("Example_Data_Markedup",  package = "qcoder"), "/", examples)
    file.copy(from = examples,
              paste0(project_name, "/documents"), recursive = TRUE )
    file.copy(system.file("example_codes/codes.csv",  package = "qcoder"),
              paste0(project_name, "/codes"))
    file.copy(system.file("units/units.csv",  package = "qcoder"),
              paste0(project_name, "/units"))
    file.copy(system.file("units/unit_document_map.csv",  package = "qcoder"),
              paste0(project_name, "/units"))
  }

  invisible(TRUE)
}

#' This launches the data-reader Shiny app
#'
#' @examples
#'  \dontrun{
#'  read_data()
#' }
#' @export
read_data <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/data-reader"))
}

#' This launches the coder Shiny app
#'
#' @examples
#' \dontrun{
#'  qcode()
#' }
#' @export
qcode <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/qcoder"))
}

#' This launches the coder custom Shiny app
#'
#' @examples
#' \dontrun{
#'   qcode_custom()
#' }
#' @export
qcode_custom <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/qcoder-custom"))
}

#' Update document
#' Updates the text field of the documents data frame, typically after
#' pressing Save button in the Shiny App.  May
#' also be used in the console.
#'
#' @param updated The updated text as a character string
#' @param docs_df_path  Location of the documents rds file.
#' @param this_doc_path  Name of record to be updated, as recorded in "doc_path"
#'         field of data frame.
#'
#' @export
do_update_document <- function(updated, docs_df_path, this_doc_path){

  qcoder::error_check(updated)

  text_df <- readRDS(docs_df_path)
  # Make an archive of the unchanged data
  time <- gsub(" ", "_", Sys.time())
  time <- gsub(":", "_", time)
  time <-gsub("-", "_", time)
  archive_path <- sub(".rds", paste0("_", time, ".rds"), docs_df_path)
  saveRDS(text_df, archive_path)
  row_num <- which(text_df[,"doc_path"] == this_doc_path)
  text_df[row_num, 2] <- updated
  # make sure this save happens
  saveRDS(text_df, file = docs_df_path)
  invisible(TRUE)
}

#'  Update document to unit links
#'  Saves or updates the links between observation units and documents
#'
#' @param checked  vector of new or updated links
#' @param data_path full path to document dataset
#' @param doc_path value of doc_path for the document
#' @param units_docs_path  full path of the data frame of unit to docs links
#'
#' @export
update_links <- function(checked = "", data_path = docs_df_path, this_doc_path = "", units_docs_path = units_docs_path){
  text_df <- readRDS(data_path)
  new_rows <- data.frame(doc_path = this_doc_path, unit_id = checked)
  # We could be removing or adding so we need to delete all the old links
  unit_doc_links <- readRDS(units_docs_path)
  unit_doc_links <- unit_doc_links %>% filter(doc_path != this_doc_path)
  unit_doc_links <- rbind(unit_doc_links, new_rows)
  saveRDS(unit_doc_links, file = units_docs_path)
  invisible(TRUE)
}
