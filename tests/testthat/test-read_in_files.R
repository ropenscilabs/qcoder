context("Reading files into data frames for analysis")

test_that("A new file is successfully added to the documents", {
  new_file_name <- "newfile.txt"
  file_list <- data.frame(name = new_file_name, size = c(NA), type = c(""),
                          datapath = c("NA/newfile6.txt"), stringsAsFactors = FALSE)
  file_path <- file.path(getwd(), "data")
  data_path <- file.path(getwd(),"data/qcoder_documents_my_qcoder_project.rds")
  save_path <- paste0(tempdir(), "/qcoder_documents_my_qcoder_project.rds")
  file.copy("./data/qcoder_documents_my_qcoder_project.rds", save_path)
  add_new_documents(files = file_list, docs_df_path = save_path, file_path = file_path)
  new_df <- readRDS(save_path)
  expect_equal(nrow(new_df), 6)
  expect_equal(new_df[6,"doc_path"], new_file_name)
  unlink(save_path, recursive = TRUE)
})

test_that("A new file with the same name as an existing file generates a warning", {
  new_file_name <- "CoC_Example2_MU.txt"
  file_list <- data.frame(name = new_file_name, size = c(NA), type = c(""), datapath = c("NA/CoC_Example2_MU.txt"))
  data_path <- "./data/"
  save_path <- paste0(tempdir(), "/qcoder_documents_my_qcoder_project.rds")
  file.copy("./data/qcoder_documents_my_qcoder_project.rds", save_path)
  expect_warning( add_new_documents(files = file_list, docs_df_path = save_path, file_path = data_path))
  unlink(save_path)
})

test_that("Creating an empty code file works.", {
  create_qcoder_project(project_name = "_qc_project")
  create_empty_code_file(file.path(getwd(),
             "_qc_project/data_frames/qcoder_codes__qc_project.rds"))
  expect_true(file.exists(file.path(getwd(),
           "_qc_project/data_frames/qcoder_codes__qc_project.rds")))
  test_data <- readRDS(file.path(getwd(),
            "_qc_project/data_frames/qcoder_codes__qc_project.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  unlink(file.path(getwd(), "_qc_project"), recursive = TRUE)
})

test_that("Creating an empty units file works.", {

  create_qcoder_project(project_name = "_qc_project")
  create_empty_units_file(file.path(getwd(),
                                   "_qc_project/data_frames/qcoder_units__qc_project.rds"))
  expect_true(file.exists(file.path(getwd(),
                                    "_qc_project/data_frames/qcoder_units__qc_project.rds")))
  test_data <- readRDS(file.path(getwd(),
                                 "_qc_project/data_frames/qcoder_units__qc_project.rds"))
  expect_equal(names(test_data), c("unit_id", "name"))
  unlink(file.path(getwd(), "_qc_project"), recursive = TRUE)
})

test_that("Creating an empty documents file works.", {

  create_qcoder_project(project_name = "_qc_project")
  create_empty_docs_file(file.path(getwd(),
              "_qc_project/data_frames/qcoder_documents__qc_project.rds"))
  expect_true(file.exists(file.path(getwd(),
              "_qc_project/data_frames/qcoder_documents__qc_project.rds")))
  test_data <- readRDS(file.path(getwd(),
               "_qc_project/data_frames/qcoder_documents__qc_project.rds"))
  expect_equal(names(test_data), c("doc_id", "document_text", "doc_path"))
  unlink(file.path(getwd(), "_qc_project"), recursive = TRUE)
})

test_that("Creating an empty unit document map file works.", {

  create_qcoder_project(project_name = "_qc_project")
  create_empty_unit_doc_file(file.path(getwd(),
           "_qc_project/data_frames/qcoder_units_document_map__qc_project.rds"))
  expect_true(file.exists(file.path(getwd(),
            "_qc_project/data_frames/qcoder_units_document_map__qc_project.rds")))
  test_data <- readRDS(file.path(getwd(),
            "_qc_project/data_frames/qcoder_units_document_map__qc_project.rds"))
  expect_equal(names(test_data), c("doc_path", "unit_id"))
  unlink(file.path(getwd(), "_qc_project"), recursive = TRUE)
})
