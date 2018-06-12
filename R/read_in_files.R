
#'  Create a data frame of documents
#' @param path to a folder contain text files to be analyzed.
#' @param The name of the RDS file that the data frame will be stored in.
#'
#' @examples
#' fp <-"/documents/"
#' dfn <- "testdata"
#' read_raw_data(fp, dfn)
#' @export
read_raw_data <- function(folder_path = "/documents/",
                          data_frame_name = "qcoder_documents",
                          project_name){
    if (!is.null(project_name)){
      folder_path <- paste0(project_name, folder_path)
      data_frame_name <- paste0(project_name, "/data_frames/",
                                paste0(data_frame_name,"-", project_name))
      }
    if (file.exists(folder_path)){
    file_list <- dir(folder_path)
    doc_text  <- character()
    for (i in 1:length(file_list)){
       doc_text[i] <- readr::read_file(paste0(folder_path,
                                             file_list[i]))
    }
  } else {
    return(sprintf("Filepath %s does not exist",folder_path))
  }

  data_set <- data.frame( doc_id = seq_along(1:length(file_list)),
                          document_text = doc_text,
                          doc_path = file_list,
                          stringsAsFactors = FALSE)
  saveRDS(data_set, file = paste0(data_frame_name,".rds" ))
  invisible(TRUE)
}

#'  Create a file of codes from csv file
#'  Use this is you have a spreadsheet of codes already created.
#'
#' @param path to a file containing  code data in csv.
#' @param The name of the RDS file that the data frame will be stored in.
#'
#' @examples
#' fp <-"inst/example_codes/"
#' dfn <- "test_codes"
#' read_code_data(fp, dfn)
#' @export
read_code_data <- function(file_path = "codes/codes.csv",
                           data_frame_name = "qcoder_codes", project_name){
  if (!is.null(project_name)){
    file_path <- paste0(project_name, "/", file_path)
    data_frame_name <- paste0(project_name, "/data_frames/",
                              paste0(data_frame_name, "_", project_name))
  }
    code_data <- readr::read_csv(file = file_path)
    # validate column names etc here
    code_data$code <- as.factor(code_data$code)
  # try catch this save
  saveRDS(code_data, file = paste0(data_frame_name,".rds" ))
  invisible(TRUE)
}

#' @export
create_empty_code_file <-function(file_path = "codes",
                                  data_frame_name = "qcoder_codes",
                                  project_name){
  if (!is.null(project_name)){
    file_path <- paste0(project_name, "/", file_path)
    data_frame_name <- paste0(project_name, "/data_frames/", data_frame_name)
  }
  cn <- c("code_id", "code", "code.description")
  code_data <- as.data.frame(matrix(data = NA,0,length(cn)))
  colnames(code_data) <- cn
  code_data$code_description <- as.character(code_data$code.description)
  code_data$code_id <- as.numeric(code_data$code_id)
  code_data$code <-as.factor(code_data$code)
  saveRDS(code_data, file = paste0(data_frame_name,".rds" ))
 invisible( TRUE)
}

#'  Create a data frame of units from csv file
#'  Use this is you have a spreadsheet of units already created.
#'
#' @param path to a file containing  unit data in csv.
#' @param The name of the RDS file that the data frame will be stored in.
#'
#' @examples
#' fp <-"units/units.csv"
#' dfn <- "my_units"
#' read_unit_data(fp, dfn)
#' @export
read_unit_data <- function(file_path = "units.csv",
                           data_frame_name = "qcoder_units", project_name){
  if (!is.null(project_name)){
    file_path <- paste0(project_name, "/units/", file_path)
    data_frame_name <- paste0(project_name, "/data_frames/",
                              paste0(data_frame_name, "_", project_name))
  }
  units <- readr::read_csv(file = file_path)
  # validate column names etc here

  # try catch this save
  saveRDS(units, file = paste0(data_frame_name,".rds" ))
  invisible(TRUE)
}

#' Define a many to many unit to document map
#' @export
create_unit_doc_file <- function(file_path, data_frame_name, project_name){
  ud <- c("unit_id", "doc_id")
  code_data <- as.data.frame(matrix(data = NA,0,length(ud)))
  colnames(unit_document_map) <- ud
  unit_document_map$code_description <- as.integer(unit_document_map$unit_id)
  unit_document_map$doc_id <- as.integer(unit_document_map$doc_id)
  saveRDS(unit_document_map, file = paste0(file_path,".rds" ))
  invisible(TRUE)
}

#' @export
import_project_data<- function(project_name){
  read_raw_data(project_name = project_name)
  read_code_data(project_name = project_name)
  read_unit_data(project_name = project_name)
}
