
#'  Create a data frame of documents
#' @param folder_path path to a folder contain text files to be analyzed.
#' @param data_frame_name The name of the RDS file that the data frame will be stored in.
#' @param project_name Name of the Qcoder project
#' @param docs_df_path Full path to the docs data frame.
#' @param project_path Full path to the project folder.
#'
#' @examples
#'  \dontrun{
#' fp <-"/documents/"
#' dfn <- "testdata"
#' read_raw_data(fp, dfn, "")
#' }
#' @export
read_raw_data <- function(folder_path = "/documents/",
                          data_frame_name = "qcoder_documents",
                          project_name = NULL,
                          docs_df_path = "",
                          project_path = ""){
    if (project_path == ""){
      project_path <- paste0(getwd(), "/", project_name)
    }
    if (!is.null(project_name) & docs_df_path == ""){
      folder_path <- paste0(project_path, folder_path)
      docs_df_path <- paste0(project_path, "/data_frames/",
                                data_frame_name, "_", project_name, ".rds" )
    }

    if (file.exists(folder_path)){
      file_list <- dir(folder_path)
      doc_text  <- character()
      # This is because not all users will be able to install textreadr.
      if (!requireNamespace("textreadr", quietly = TRUE)){
        for (i in 1:length(file_list)){
          doc_text[i] <- readr::read_file(paste0(folder_path, file_list[i]))
        }
      } else {
        for (i in 1:length(file_list)){
           doc_text[i] <- textreadr::read_document(
                                  paste0( folder_path, file_list[i]),
                                  combine = TRUE)
        }
      }
    } else {
      return(sprintf("Filepath %s does not exist", folder_path))
    }

  data_set <- data.frame( doc_id = seq_along(1:length(file_list)),
                          document_text = doc_text,
                          doc_path = file_list,
                          stringsAsFactors = FALSE)
  saveRDS(data_set, file = docs_df_path)
  invisible(TRUE)
}

#' Add new documents
#' Adds new document or documents to an existing documents data frame.
#' @param files  file tibble produced by ShinyFiles
#' @param file_path  Full path to the data set of documents including trailing slash
#' @param docs_df_path  Existing data frame of text documents
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
                       doc_text[i] <- textreadr::read_document(paste0(file_path,
                                                 file_list[i]))
          }
        }
        ids <- integer(length(file_list))
        new_rows <- data.frame(doc_id = ids, document_text = doc_text, doc_path = file_list)
        text_df <- rbind( text_df, new_rows)
        row_n <- row.names(text_df)
        text_df$doc_id <- ifelse(text_df$doc_id == 0, row_n,
                                          text_df$doc_id)

      saveRDS(text_df, file = docs_df_path )
      invisible(TRUE)
}

#'  Create a file of codes from csv file
#'  Use this is you have a spreadsheet of codes already created.
#'
#' @param file_path Path to a file containing  code data in csv.
#' @param data_frame_name The name of the RDS file that the data frame will be stored in.
#' @param project_name Name of the project, which matches folder name
#' @param project_path Full path to the project folder
#' @param codes_df_path Full path to the codes data frame.
#'
#' @examples
#'  \dontrun{
#' fp <-"inst/example_codes/"
#' dfn <- "test_codes"
#' read_code_data(fp, dfn)
#' }
#' @export
read_code_data <- function(file_path = "codes/codes.csv", codes_df_path = "",
                           data_frame_name = "qcoder_codes", project_name, project_path = ""){
    if (project_path == ""){
        project_path <- paste0(getwd(), "/", project_name)
    }

    if (!is.null(project_name)){
      file_path <- paste0(project_path, "/", file_path)
      codes_df_path <- paste0(project_path, "/data_frames/",
                                data_frame_name, "_", project_name, ".rds" )
    }

  if (file.exists(file_path)){
      code_data <- readr::read_csv(file = file_path,
                                  col_types = "icc")
      # validate column names etc here
      #code_data$code <- as.character(code_data$code)
   } else {
      code_data <- create_empty_code_file(project_name = project_path , codes_df_path = codes_df_path)
   }

  # try catch this save
  saveRDS(code_data, file = codes_df_path)
  invisible(TRUE)
}

#' Create an empty codes data set
#'
#' Used to create a codes data frame with no data but that can
#' have data added.
#'
#' @param data_frame_name Name of the data frame to be created
#' @param project_name Name of the project that the codes are associated with
#' @param file_path Path to the file location to be used (under project root).
#'                  defaults to "data_frames".
#' @param codes_df_path The full path to the codes data frame
#' @param project_path  The full path to the project folder
#'
#' @export
create_empty_code_file <-function( data_frame_name = "qcoder_codes",
                                   codes_df_path = "",
                                   project_path = "",
                                   file_path = "data_frames",
                                   project_name = ""){
  if (project_path == "" & project_name != ""){
    project_path <- paste0(getwd(), project_name)
  }
  if (project_name != ""){
    file_path <- paste0(project_path, "/", file_path)
    codes_df_path <- paste0(file_path, "/",
                              data_frame_name, "_", project_name, ".rds" )
  }

  cn <- c("code_id", "code", "code.description")
  code_data <- as.data.frame(matrix(data = NA, 0, length(cn)))
  colnames(code_data) <- cn
  code_data$code_description <- as.character(code_data$code.description)
  code_data$code_id <- as.integer(code_data$code_id)
  code_data$code <-as.character(code_data$code)
  saveRDS(code_data, file = codes_df_path)

}

#'  Create a data frame of units from csv file
#'  Use this is you have a spreadsheet of units already created.
#'
#' @param file_path path to a file containing  unit data in csv.
#' @param data_frame_name The name of the RDS file that the data frame will be stored in.
#' @param project_name  Name of project if available
#' @param project_path Full path to the project folder.
#' @param units_df_path Full path to the units data frame.
#'
#' @examples
#'  \dontrun{
#' fp <-"units/units.csv"
#' dfn <- "my_units"
#' read_unit_data(fp, dfn)
#' }
#' @export
read_unit_data <- function(file_path = "units.csv",
                           data_frame_name = "qcoder_units",
                           project_name,
                           project_path = "",
                           units_df_path = ""){
  if (project_path == ""){
    project_path <- paste0(getwd(), "/", project_name)
  }

  if (!is.null(project_name) & units_df_path == ""){
    file_path <- paste0(project_path, "/units/", file_path)
    units_df_path <- paste0(project_path, "/data_frames/",
                              data_frame_name, "_", project_name, ".rds" )
  }
  units <- readr::read_csv(file = file_path,
                           col_types = "ic" )
  # validate column names etc here

  # try catch this save
  saveRDS(units, file = units_df_path)
  invisible(TRUE)
}

#'  Create a data frame of unit to document links from csv file
#'  Use this is you have a spreadsheet already created.
#'
#' @param file_path path to a file containing  unit document map data in csv.
#' @param data_frame_name The name of the RDS file that the data frame will be stored in.
#' @param project_name  Name of project if available
#' @param project_path Full path to the project folder
#' @param units_docs_path Full path to the documents data frame.
#'
#' @examples
#'  \dontrun{
#' fp <-"units/units.csv"
#' dfn <- "my_units"
#' read_unit_data(fp, dfn)
#' }
#' @export
read_unit_document_map_data <- function(file_path = "unit_document_map.csv",
                           data_frame_name = "qcoder_unit_document_map",
                           project_name,
                           project_path = "",
                           units_docs_path = ""){
  if (project_path == ""){
    project_path <- paste0(getwd(), "/", project_name)
  }
  if (!is.null(project_name)){
    file_path <- paste0(project_path, "/units/", file_path)
    data_frame_path <- paste0("/data_frames/",
                              paste0(data_frame_name, "_", project_name ))
  }

  qcoder_unit_document_map <- readr::read_csv(file = file_path,
                                              col_types = readr::cols(doc_path = "c",
                                                                unit_id = "i"))
  # validate column names etc here

  if (units_docs_path == ""){
    units_docs_path <- paste0(project_path, data_frame_path, ".rds" )
  }

  # try catch this save
  saveRDS(qcoder_unit_document_map, file = units_docs_path)
  invisible(TRUE)
}

#' Define a many to many unit to document map
#' @param file_path Path to store data frame on.
#' @param data_frame_name Name of the data frame that will contain the map
#' @param project_name Name of the project, if it exists
#'
#' @export
create_unit_doc_file <- function(file_path = "data_frames",
                                 data_frame_name = "unit_document_map",
                                 project_name){
  ud <- c("doc_path", "unit_id")
  code_data <- as.data.frame(matrix(data = NA, 0, length(ud)))
  colnames(unit_document_map) <- ud
  unit_document_map$doc_path <- as.character(unit_document_map$doc_path)

  unit_document_map$unit_id <- as.integer(unit_document_map$unit_id)
  saveRDS(unit_document_map, file = paste0(file_path,".rds" ))
  invisible(TRUE)
}

#' Read data into a project
#' Convenience method to read raw data from standard locations and using
#' standard names in a project folder structure.
#'
#' @param project_name The project name. This should represent the folder
#'                     holding the project.
#' @export
import_project_data<- function(project_name){
  read_raw_data(project_name = project_name)
  read_code_data(project_name = project_name)
  read_unit_data(project_name = project_name)
  read_unit_document_map_data(project_name = project_name)
}
