#' Build the paths for file creation
#'
#' Builds the paths to the data to be imported and to the data
#' frame where the imported data is to be stored.
#' The project name is required, all other parameters may be set.
#' These each represent a segment of the path to a file.
#' If a project path is not set or set to "" it will be set to
#' the current working directory via getwd().
#' The Shiny qcode application assumes that the data frame folder will
#' be "data_frames" and that any new documents to be imported will
#' be in a folder called "documents".
#'
#' @param project_name Name of the project
#' @param data_frame_name Name of the data frame that will contain the data
#' @param data_path Path segment to the data. The format for this may
#'      depend on the function using the paths.
#' @param df_path path segment(s) to the created data frame file
#'                 from the project path
#' @param project_path Path to the project (not including project_name). This
#'   will be set to getwd() if a value of "" is passed in.
#' @return A named list of paths. "data_frame_path" is the path to the data frame
#'         and "data" is the path to the data.
build_paths <- function(project_name,
                              data_path = "",
                              data_frame_name = "",
                              df_path = "data_frames",
                              project_path = ""
                        ) {
  if (project_path == ""){
    project_path <- getwd()
  }
  if ( df_path == ""){
    df_path <- "data_frames"
  }
    dir.create(file.path(project_path, project_name, df_path),
               recursive = TRUE, showWarnings = FALSE)
    df_path <- file.path(project_path, project_name, df_path,
                      paste0(data_frame_name, "_", project_name, ".rds" ))
    paths <- list("data_frame_path" = df_path)
    data_path <- file.path(project_path, project_name, data_path)
    paths[["data"]] <- data_path

 paths
}

#'  Create a data frame of documents
#' @param project_name Name of the Qcoder project
#' @param data_path path to a folder contain text files to be analyzed.
#' @param data_frame_name The name of the RDS file that the data frame
#' will be stored in.
#' @param df_path Full path to the docs data frame.
#' @param project_path Full path to the project folder.
#'
#' @examples
#'  \dontrun{
#' read_documents_data("_my_qcoder_project")
#' }
#' @export
read_documents_data <- function(project_name,
                          data_path = "documents/",
                          df_path = "data_frames",
                          data_frame_name = "qcoder_documents",
                          project_path = ""){
    paths <- build_paths(project_name = project_name, data_path = data_path,
                           df_path = df_path, project_path = project_path,
                           data_frame_name = data_frame_name)
    if (length(dir(paths$data)) != 0){
      file_list <- dir(paths$data)
      doc_text  <- character()
      # This is because not all users will be able to install textreadr.
      if (!requireNamespace("textreadr", quietly = TRUE)){
        for (i in 1:length(file_list)){
          doc_text[i] <- readr::read_file(paste0(paths$data, file_list[i]))
        }
      } else {
        for (i in 1:length(file_list)){
           doc_text[i] <- textreadr::read_document(
                                  paste0( paths$data, file_list[i]),
                                  combine = TRUE)
        }
      }

        data_set <- data.frame( doc_id = seq_along(1:length(file_list)),
                          document_text = doc_text,
                          doc_path = file_list,
                          stringsAsFactors = FALSE)
        #print(paths[["data_frame_path"]])
        
        # validate column names etc here
        actualNames <- names(data_set)
        expectedNames <- c("doc_id", "document_text", "doc_path") #GOOD
        if (length(expectedNames %in% actualNames) != length(data_set)){
          warning("Required variables for documents_data are not present")
        }
        
        
        saveRDS(data_set, file = paths[["data_frame_path"]])
    } else {
      create_empty_docs_file(paths[["data_frame_path"]])
  }
  invisible(TRUE)
}

#' Create an empty documents data set
#'
#' Used to create a codes data frame with no data but that can
#' have data added. File is placed in the data_frames folder.
#'
#' @param path Full path to data frame to be created.
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project")
#' path <- file.path(getwd(),
#'   "_my_qcoder_project/data_frames/qcoder_docs__my_qcoder_project")
#' create_empty_docs_file(path)
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
create_empty_docs_file <-function(path){

  cn <- c("doc_id", "document_text", "doc_path")
  doc_data <- as.data.frame(matrix(data = NA, 0, length(cn)))
  colnames(doc_data) <- cn
  doc_data$document_text <- as.character(doc_data$document_text)
  doc_data$doc_id <- as.integer(doc_data$doc_id)
  doc_data$doc_path <-as.character(doc_data$doc_path)

  saveRDS(doc_data, file = path)
}

#' Add new documents
#' Adds new document or documents to an existing documents data frame.
#' @param files  file tibble produced by ShinyFiles
#' @param file_path  Full path to the data set of documents including
#' trailing slash
#' @param docs_df_path  Existing data frame of text documents
#' @examples
#' create_qcoder_project(project_name = "my_qcoder_project", sample = TRUE)
#'
#' unlink("./my_qcoder_project", recursive=TRUE)
#'
#' @export
add_new_documents <- function(files, docs_df_path = "", file_path = ""){
        text_df <- readRDS(docs_df_path)
        file_list <- files[["name"]]
        old_docs <- text_df[["doc_path"]]
        if (length(intersect(file_list, old_docs)) != 0){
          warning("One or more files are already imported")
          return()
        }
        doc_text  <- character()
        if (!requireNamespace("textreadr", quietly = TRUE)){
          for (i in 1:length(file_list)){
            doc_text[i] <- readr::read_file(paste0(file_path,
                                                           file_list[i]))
          }
        } else {
          for (i in 1:length(file_list)){
                       if (length(file_list) == 0){
                         return()
                       }
                       doc_text[i] <- textreadr::read_document(
                                    paste0(file_path, file_list[i]))
          }
        }
        ids <- integer(length(file_list))
        new_rows <- data.frame(doc_id = ids, document_text = doc_text,
                               doc_path = file_list)
        text_df <- rbind( text_df, new_rows)
        row_n <- row.names(text_df)
        text_df$doc_id <- ifelse(text_df$doc_id == 0, row_n,
                                          text_df$doc_id)

      saveRDS(text_df, file = docs_df_path )
      invisible(TRUE)
}

#'  Create a file of codes from csv file
#'  Use this if you have a spreadsheet of codes already created.
#'
#' @param data_path Path to a file containing  code data in csv.
#' @param data_frame_name The name of the RDS file that the data frame
#'                         will be stored in.
#' @param project_name Name of the project, which matches folder name
#' @param project_path Full path to the project folder
#' @param df_path Full path to the codes data frame.
#'
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project", sample = TRUE)
#' read_code_data(project_name = "_my_qcoder_project")
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
read_code_data <- function(project_name,
                           data_path = "codes/codes.csv",
                           df_path = "data_frames",
                           data_frame_name = "qcoder_codes",
                           project_path = ""){
  paths <- build_paths(project_name = project_name, data_path = data_path,
                                 df_path = df_path, project_path = project_path,
                                 data_frame_name = data_frame_name)

  if (file.exists(data_path)){
      code_data <- readr::read_csv(file = paths[["data"]],
                                  col_types = "icc")
      # validate column names etc here
      actualNames <- names(code_data)
      expectedNames <- c("code_id", "code", "code.description")  #GOOD
      
      if (length(expectedNames %in% actualNames) != length(code_data)){
        warning("Required variables for read_code_data are not present")
      }
      
      #code_data$code <- as.character(code_data$code)

      # try catch this save
      saveRDS(code_data, file = paths[["data_frame_path"]])

   } else {
      create_empty_code_file(paths[["data_frame_path"]])
   }

   invisible(TRUE)
}

#' Create an empty codes data set
#'
#' Used to create a codes data frame with no data but that can
#' have data added. File is placed in the data_frames folder.
#'
#' @param path Full path to data frame to be created.
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project")
#' path <- file.path(getwd(),
#'   "_my_qcoder_project/data_frames/qcoder_codes__my_qcoder_project")
#' create_empty_docs_file(path)
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
create_empty_code_file <-function( path){
  cn <- c("code_id", "code", "code.description")
  code_data <- as.data.frame(matrix(data = NA, 0, length(cn)))
  colnames(code_data) <- cn
  code_data$code.description <- as.character(code_data$code.description)
  code_data$code_id <- as.integer(code_data$code_id)
  code_data$code <-as.character(code_data$code)

  saveRDS(code_data, file = path)
}

#'  Create a data frame of units from csv file
#'  Use this is you have a spreadsheet of units already created.
#'
#' @param data_path path to a file containing  unit data in csv.
#' @param data_frame_name The name of the RDS file that the data frame
#' will be stored in.
#' @param project_name  Name of project if available
#' @param project_path Full path to the project folder.
#' @param df_path Full path to the units data frame.
#'
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project", sample = TRUE)
#' read_unit_data(project_name = "_my_qcoder_project")
#' unlink("./_my_qcoder_project", recursive=TRUE)
#'
#' @export
read_unit_data <- function(data_path = "units/units.csv",
                           data_frame_name = "qcoder_units",
                           project_name,
                           project_path = "",
                           df_path = "data_frames"){
  paths <- build_paths(project_name = project_name, data_path = data_path,
                             df_path = df_path, project_path = project_path,
                             data_frame_name = data_frame_name)

  if (file.exists(paths[["data"]])){
        # units_df_path <- file.path(df_path,
        #                   paste0(data_frame_name, "_", project_name, ".rds" ))
        units <- readr::read_csv(file = paths[["data"]],
                           col_types = "ic" )
  
  # validate column names etc here
  actualNames <- names(units)
  expectedNames <- c("unit_id","name")    #GOOD
  if (length(expectedNames %in% actualNames) != length(units)){
    warning("Required variables for read_unit_data are not present")
  }
  

  # try catch this save
      saveRDS(units, file = paths[["data_frame_path"]])
  } else {
    create_empty_units_file(paths[["data_frame_path"]])
  }
  invisible(TRUE)
}

#' Define an empty  units data frame
#' @param path Full path to data frame to be created.
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project")
#' path <- file.path(getwd(),
#'   "_my_qcoder_project/data_frames/qcoder_units__my_qcoder_project")
#' create_empty_docs_file(path)
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
create_empty_units_file <- function(path){
  ud <- c("unit_id","name")
  units <- as.data.frame(matrix(data = NA, 0, length(ud)))
  colnames(units) <- ud
  units$name <- as.character(units$name)
  units$unit_id <- as.integer(units$unit_id)
  saveRDS(units, file = path)
  invisible(TRUE)
}

#'  Create a data frame of unit to document links from csv file
#'  Use this is you have a spreadsheet already created.
#'
#' @param project_name  Name of project
#' @param data_path path to a file containing  unit document map data in csv.
#' @param data_frame_name The name of the RDS file that the data frame
#' will be stored in.
#' @param project_path Full path to the project folder
#' @param df_path Full path to the documents data frame.
#'
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project", sample = TRUE)
#' project_name = "_my_qcoder_project"
#' read_unit_document_map_data( project_name = "_my_qcoder_project")
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
read_unit_document_map_data <- function(project_name,
                           data_path = "units/unit_document_map.csv",
                           data_frame_name = "qcoder_unit_document_map",
                           project_path = "",
                           df_path = "data_frames"){
    paths <- build_paths(project_name = project_name, data_path = data_path,
                               df_path = df_path, project_path = project_path,
                               data_frame_name = data_frame_name)
    if (file.exists(paths[["data"]])){

          qcoder_unit_document_map <- readr::read_csv(file = file_path,
                                              col_types = readr::cols(doc_path = "c",
                                                                unit_id = "i"))
          # validate column names etc here
          actualNames <- names(qcoder_unit_document_map)
          expectedNames <- c("doc_path", "unit_id")
  
          if (length(expectedNames %in% actualNames) != length(qcoder_unit_document_map)){
            warning("Required variables for read_unit_document_map_data are not present")
          }

             

          saveRDS(qcoder_unit_document_map, file = paths[["data_frame_path"]])
        } else {
          create_empty_unit_doc_file(paths[["data_frame_path"]])
        }
  invisible(TRUE)
}

#' Define an empty many to many unit to document map
#' @param path Full path to data frame to be created.
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project")
#' path <- file.path(getwd(),
#'   "_my_qcoder_project/data_frames/qcoder_units_document_map__my_qcoder_project")
#' create_empty_docs_file(path)
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
create_empty_unit_doc_file <- function(path ){
  ud <- c("doc_path", "unit_id")
  unit_document_map <- as.data.frame(matrix(data = NA, 0, length(ud)))
  colnames(unit_document_map) <- ud
  unit_document_map$doc_path <- as.character(unit_document_map$doc_path)

  unit_document_map$unit_id <- as.integer(unit_document_map$unit_id)
  saveRDS(unit_document_map, file = path)
  invisible(TRUE)
}

#' Read data into a project
#' Convenience method to read raw data from standard locations and using
#' standard names in a project folder structure.
#'
#' @param project_name The project name. This should represent the folder
#'                     holding the project.
#' @examples
#' create_qcoder_project(project_name = "_my_qcoder_project", sample = TRUE)
#' import_project_data("_my_qcoder_project")
#' unlink("./_my_qcoder_project", recursive=TRUE)
#' @export
import_project_data <- function(project_name){
  read_documents_data(project_name = project_name)
  read_code_data(project_name = project_name)
  read_unit_data(project_name = project_name)
  read_unit_document_map_data(project_name = project_name)
}
