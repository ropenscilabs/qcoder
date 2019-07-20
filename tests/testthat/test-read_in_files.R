context("Reading files into data frames for analysis")

test_that("A new file is successfully added to the documents", {
  new_file_name <- "newfile.txt"
  file_list <- data.frame(name = new_file_name, size = c(NA), type = c(""), datapath = c("NA/newfile6.txt"))
  data_path <- "./data/"
  save_path <- paste0(tempdir(), "/rqcoder_documents_my_qcoder_project.rds")
  file.copy("./data/qcoder_documents_my_qcoder_project.rds", save_path)
  add_new_documents(files = file_list, file_path = data_path, docs_df_path = save_path)
  new_df <- readRDS(save_path)
  expect_equal(nrow(new_df), 6)
  expect_equal(new_df[6,"doc_path"], new_file_name)
  unlink(save_path)
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

test_that("Creating an empty code file works with only a df path and project name.", {
  create_empty_code_file(project_name = "_qc_project",
         df_path = "data_frames2")
  expect_true(file.exists(file.path(getwd(),
                          "_qc_project/data_frames2/qcoder_codes__qc_project.rds")))
  test_data <- readRDS(file.path(getwd(),
                          "_qc_project/data_frames2/qcoder_codes__qc_project.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  unlink(paste0(getwd(), "_qc_project/data_frames2/qcoder_codes__qc_project.rds"))
})

test_that("Creating an empty code file works with only a project path
          and project name.", {
  dir.create(paste0(getwd(), "/my_qcoder_path"))
  create_empty_code_file(project_name = "_qc_project",
                          project_path =
                          paste0(getwd(), "/my_qcoder_path")
                          )
  expect_true(file.exists(paste0(getwd(),
              "/my_qcoder_path/_qc_project/data_frames/qcoder_codes__qc_project.rds")))
  test_data <- readRDS(paste0(getwd(),
                              "/my_qcoder_path/_qc_project/data_frames/qcoder_codes__qc_project.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  unlink(paste0(getwd(), "/my_qcoder_path"), recursive = TRUE)
})

test_that("Creating an empty code file works with no project path.", {
  create_empty_code_file(project_name = "_qc_project")
  expect_true(file.exists(file.path(getwd(), "_qc_project/data_frames/qcoder_codes__qc_project.rds")))
  test_data <- readRDS(file.path(getwd(), "_qc_project/data_frames/qcoder_codes__qc_project.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  # Be careful running this
  unlink(paste0(getwd(), "_qc_project"), recursive = TRUE)
})

test_that("Creating an empty code file works with both a project path
          and a project name.", {
  create_empty_code_file(project_path =
                           paste0(tempdir(), "/my_qcoder_project"),
                           project_name = "other_project_name")
  expect_true(file.exists(paste0(tempdir(),
  "/my_qcoder_project/other_project_name/data_frames/qcoder_codes_other_project_name.rds")))
  test_data <- readRDS(paste0(tempdir(),
                 "/my_qcoder_project/other_project_name/data_frames/qcoder_codes_other_project_name.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  unlink(paste0(tempdir(), "/my_qcoder_project"), recursive = TRUE)
})
