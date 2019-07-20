#' Create a standard set of folders for a QCoder project
#'
#' @param project_name  A string project name to be located in the
#'                      current working directory or a path to a project folder.
#' @param sample Logical that indicates that the sample data should be copied to the project.
#' @examples
#' create_qcoder_project(project_name = "my_qcoder_project")
#' unlink("./my_qcoder_project", recursive=TRUE)
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
#' if (interactive()) {
#'  qcode()
#'}
#' @export
qcode <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/qcoder"), quiet = TRUE)
}

#' This launches the coder custom Shiny app
#'
#' @examples
#' if (interactive()) {
#'   qcode_custom()
#' }
#' @export
qcode_custom <- function() {
  package_location <- system.file(package = "qcoder")
  shiny::runApp(paste0(package_location, "/shiny/qcoder-custom"), quiet = TRUE)
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
#' @examples
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
do_update_document <- function(updated, docs_df_path, this_doc_path){

  qcoder::error_check(updated)
  path <- docs_df_path
  text_df <- readRDS(path)
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
#' @param docs_df_path full path to document dataset
#' @param this_doc_path value of doc_path for the document
#' @param units_docs_path  full path of the data frame of unit to docs links
#' @examples
#'
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
update_links <- function(checked = "", docs_df_path = "", this_doc_path = "", units_docs_path = ""){
  text_df <- readRDS(docs_df_path)
  new_rows <- data.frame(doc_path = this_doc_path, unit_id = checked)
  # We could be removing or adding so we need to delete all the old links
  unit_doc_links <- readRDS(units_docs_path)
  unit_doc_links <- unit_doc_links["doc_path" != this_doc_path]
  unit_doc_links <- rbind(unit_doc_links, new_rows)
  saveRDS(unit_doc_links, file = units_docs_path)
  invisible(TRUE)
}

#' Add unit
#' Append a new unit record to the existing data frame
#' @param units_df Existing units data frame
#' @param new_unit  text name of a new unit (single name only)
#' @param units_df_path  full path to the units data frame
#' @examples
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export

add_unit <- function(units_df, new_unit, units_df_path){
  if (new_unit %in% units_df$name){
    warning("A unit with the name already exists, please choose a unique name.")
    return()
  }
  new_id <- max(units_df$unit_id) +1
  new_row <- data.frame("unit_id" = new_id, "name" = new_unit)
  units_df <- rbind(units_df, new_row)
  saveRDS(units_df, file = units_df_path)
  invisible(TRUE)
}

#' Add code
#' Append a new unit record to the existing data frame
#' @param codes_df Existing codes data frame
#' @param new_code  text name of a new code (single name only)
#' @param new_code_desc  text description of the code
#' @param codes_df_path  full path to the codes data frame
#' @examples
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export

add_code <- function(codes_df, new_code, new_code_desc, codes_df_path){
  if (new_code %in% codes_df$code){
    warning("A code with the name already exists, please choose a unique name.")
    return()
  }
  new_id <- as.integer(max(codes_df$code_id) +1)
  new_row <- data.frame("code_id" = new_id, "code" = new_code,
                        "code.description" = new_code_desc)
  codes_df <- rbind(codes_df, new_row)
  saveRDS(codes_df, file = codes_df_path)
  invisible(TRUE)
}

#' Check for a valid qcoder project
#'
#' @param path_to_test Path to possible project folder
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project")
#' validate_project("_my_qcoder_project")
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @return NULL for valid project, Error otherwise.
#' @export
validate_project <- function(path_to_test){
  shiny::validate(shiny::need(assertthat::is.dir(path_to_test),
                          message = "Invalid project path: Not a directory",
                          label = "project dir"))
  shiny::validate(shiny::need(assertthat::is.writeable(path_to_test),
                          message = "Invalid project path: Path is not writeable",
                          label = "not writeable"))
  required_folders <-c("codes", "data_frames", "documents", "units")
  shiny::validate(shiny::need(all(paste0(path_to_test, "/",
                          required_folders) %in%
                                     list.dirs(path_to_test)),
                          message = "Invalid project path: Required folders are missing",
                          label = "folders missing"))
  # shiny::validate(shiny::need(assertthat::is.writeable(
  #                         paste0(path_to_test, "/data_frames")),
  #                         message = "Invalid project path: The data_frames path is not writeable"),
  #                         label = "data frames not writeable" )

}

#' Check for required imported data frames.
#'
#' @param path_to_test Path to possible project folder
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project", sample = TRUE)
#' import_project_data("_my_qcoder_project")
#' validate_project_files("_my_qcoder_project")
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @return NULL for valid project, Error otherwise.
#' @export
validate_project_files <- function(path_to_test){
  shiny::validate(shiny::need(file.exists(
    paste0(path_to_test, "/data_frames/qcoder_documents_",
           basename(path_to_test), ".rds")),
    message =
      "No documents data frame: Import data before starting qcoder.",
    label = "no documents df"))
  shiny::validate(shiny::need(file.exists(
    paste0(path_to_test, "/data_frames/qcoder_codes_",
           basename(path_to_test), ".rds")),
    message =
      "No codes data frame: Import data before starting qcoder.",
    label = "no codes df"))
  shiny::validate(shiny::need(file.exists(
    paste0(path_to_test, "/data_frames/qcoder_units_",
           basename(path_to_test), ".rds")),
    message =
      "No units data frame: Import data before starting qcoder.",
    label = "no units df"))
  shiny::validate(shiny::need(file.exists(
    paste0(path_to_test, "/data_frames/qcoder_unit_document_map_",
           basename(path_to_test), ".rds")),
    message =
      "No unit document map data frame: Import data before starting qcoder.",
    label = "no unit_document_map df"))
}


#' Format text as HTML
#' Minimal conversion of a text to html
#' @param text text to be converted
#' @examples
#' txt2html("The quick brown (QCODE)fox(/QCODE){#animal} jumped over ")
#' @export
txt2html <- function(text){
  text <- paste0("<p>", text, "</p>")
  text <- gsub("[\r\n]", "</p><p>", text)
  text <- gsub("(QCODE)", "<mark>", text, fixed = TRUE)
  text <- gsub("(/QCODE)", "</mark>", text, fixed = TRUE)
  text
}
