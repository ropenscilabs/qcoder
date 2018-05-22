
# This is a preprocessing step to create a data frame of documents
#' @param path to a folder contain text files to be analyzed.
#' @param The name of the RDS file that the data frame will be stored in.
#'
#' @examples
#' fp <-"inst/Example_Data/"
#' dfn <- "testdata"
#' read_raw_data(fp, dfn)
#' @export
read_raw_data <- function(folder_path, data_frame_name){
  file_list <- dir(folder_path)
  doc_text  <- character()
  for (i in 1:length(file_list)){
     doc_text[i]<- readr::read_file(paste0(folder_path,
                                           file_list[i]))
  }

  data_set <- data.frame( doc_id = seq_along(1:length(file_list)),
                                document_text = as.character(doc_text))
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
#' read_tag_data(fp, dfn)
#' @export
read_tag_data <- function(file_path, data_frame_name){

    code_data <- readr::read_csv(file = file_path)
    # validate column names etc here
    code_data$code <- as.factor(code_data$code)
  saveRDS(code_data, file = paste0(data_frame_name,".rds" ))
  invisible(TRUE)
}
#' @export
create_empty_code_file <-function(file_path, data_frame_name){
  cn <- c("code_id", "code", "code.description")
  code_data <- as.data.frame(matrix(data = NA,0,length(cn)))
  colnames(code_data) <- cn
  code_data$code_description <- as.character(code_data$code.description)
  code_data$code_id <- as.numeric(code_data$code_id)
  code_data$code <-as.factor(code_data$code)
  saveRDS(code_data, file = paste0(file_path,".rds" ))
}
