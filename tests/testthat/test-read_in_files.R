context("Reading files into data frames for analysis")

test_that("A new file is successfully added to the documents", {
  new_file_name <- "newfile.txt"
  file_list <- data.frame(name = new_file_name, size = c(NA), type = c(""), datapath = c("NA/newfile6.txt"))
  file_path <- "./data/"
  save_path <- paste0(tempdir(), "/rqcoder_documents_my_qcoder_project.rds")
  file.copy("./data/qcoder_documents_my_qcoder_project.rds", save_path)
  add_new_documents(files = file_list, file_path = file_path, docs_df_path = save_path)
  new_df <- readRDS(save_path)
  expect_equal(nrow(new_df), 6)
  expect_equal(new_df[6,"doc_path"], new_file_name)
  unlink(save_path)
})

test_that("A new file with the same name as an existing file generates a warning", {
  new_file_name <- "CoC_Example2_MU.txt"
  file_list <- data.frame(name = new_file_name, size = c(NA), type = c(""), datapath = c("NA/CoC_Example2_MU.txt"))
  file_path <- "./data/"
  save_path <- paste0(tempdir(), "/rqcoder_documents_my_qcoder_project.rds")
  file.copy("./data/qcoder_documents_my_qcoder_project.rds", save_path)
  expect_warning( add_new_documents(files = file_list, docs_df_path = save_path, file_path = file_path))
  unlink(save_path)
})
