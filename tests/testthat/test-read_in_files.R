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
  save_path <- paste0(tempdir(), "/qcoder_documents_my_qcoder_project.rds")
  file.copy("./data/qcoder_documents_my_qcoder_project.rds", save_path)
  expect_warning( add_new_documents(files = file_list, docs_df_path = save_path, file_path = file_path))
  unlink(save_path)
})

test_that("Creating an empty code file works with only a save path.", {
  create_empty_code_file(codes_df_path =
        paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds"))
  expect_true(file.exists(paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds")))
  test_data <- readRDS(paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  unlink(paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds"))
})

test_that("Creating an empty code file works with only a project path.", {
  create_empty_code_file(project_path =
                          paste0(tempdir(), "/my_qcoder_project")
                          )
  expect_true(file.exists(paste0(tempdir(), "/my_qcoder_project/data_frames/qcoder_codes_.rds")))
  test_data <- readRDS(paste0(tempdir(),
                              "/my_qcoder_project/data_frames/qcoder_codes_.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  unlink(paste0(tempdir(), "/data_frames"), recursive = TRUE)
})

test_that("Creating an empty code file works with no project path and
          no project name.", {
  create_empty_code_file()
  expect_true(file.exists(paste0(getwd(), "/data_frames/qcoder_codes_.rds")))
  test_data <- readRDS(paste0(getwd(), "/data_frames/qcoder_codes_.rds"))
  expect_equal(names(test_data), c("code_id", "code", "code.description"))
  # BE careful running this
  unlink(paste0(getwd(), "/data_frames"), recursive = TRUE)
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
