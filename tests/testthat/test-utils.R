context("Carry out utility functions")

basewd <- getwd()

test_that("Project with no sample data is created correctly.", {
  setwd(tempdir())
  create_qcoder_project("project1")
  dirs <- list.dirs("./project1")
  expect_equal(dirs, c("./project1", "./project1/codes",
                       "./project1/data_frames", "./project1/documents",
                       "./project1/images", "./project1/media",
                       "./project1/memos", "./project1/misc",
                       "./project1/units"))
  unlink("./project1", recursive = TRUE)
  setwd(basewd)
})

test_that("Project with sample data is created correctly.", {
  setwd(tempdir())
  create_qcoder_project("project2", sample = TRUE)
  dirs <- list.dirs("./project2")
  files <- list.files("./project2", recursive = TRUE, include.dirs = FALSE)
  expect_equal(length(setdiff(files, c("codes/codes.csv",
          "documents/CoC_Example1_mod_MU.txt", "documents/CoC_Example1_MU.txt",
          "documents/CoC_Example2_MU.txt", "documents/CoC_Example3_MU.txt",
          "documents/CoC_Example4_MU.txt", "units/units.csv",
          "units/unit_document_map.csv") )), 0)
  expect_equal(dirs, c("./project2", "./project2/codes",
                       "./project2/data_frames", "./project2/documents",
                       "./project2/images", "./project2/media",
                       "./project2/memos", "./project2/misc",
                       "./project2/units"))
  unlink("./project2", recursive = TRUE)
  setwd(basewd)
})

test_that("Updating documents works", {
  # Set up data
  qcoder_documents_my_qcoder_project <- readRDS("./data/qcoder_documents_my_qcoder_project.rds")
  save_path <- paste0(tempdir(), "/", "qcoder_documents_my_qcoder_project.rds")
  # Do the test on a copy of the data.
  saveRDS(qcoder_documents_my_qcoder_project, file = save_path)
  updated_data <- as.character("A B C D")
  do_update_document(updated_data, docs_df_path = save_path, "CoC_Example1_MU.txt")
  input <- readRDS(file = save_path)
  expect_equal(updated_data, input[2, 2])
  # This should be 2 because we started with an empty directory.
  expect_equal(length(list.files(tempdir())), 2)

  unlink(save_path)
  setwd(basewd)
})

test_that("Converting to HTML works", {
  text <- "This is a test (QCODE)highlighted(/QCODE){#something} more text"
  expected <- "<p>This is a test <mark>highlighted</mark>{#something} more text</p>"
  expect_equal(txt2html(text), expected)
})

test_that("Converting to HTML with two lines works and multiple tags works", {
  text <- "This is a test (QCODE)highlighted(/QCODE){#something} more text
  This is some more  (QCODE)highlighted(/QCODE){#something} text"
  e1 <- "<p>This is a test <mark>highlighted</mark>{#something} more text</p>"
  e2 <- "<p>  This is some more  <mark>highlighted</mark>{#something} text</p>"
  expected <- paste0(e1, e2)
  expect_equal(txt2html(text), expected)
})

test_that("Adding a code works", {
  new_code <- "test"
  new_description <- "new code description"
  codes_df_test <- readRDS("data/qcoder_codes_my_qcoder_project.rds")
  add_code(codes_df_test, new_code, new_description,
           paste0(tempdir(),"/codes_test.rds"))
  codes_df_test <- readRDS(paste0(tempdir(),"/codes_test.rds"))
  expect_equal(tail(codes_df_test, 1),
               data.frame(code_id = as.integer(7),
                          code = "test",
                          code.description ="new code description",
                          stringsAsFactors = FALSE))
  unlink(paste0(tempdir(), "/codes_test.rds"))
})

test_that("Adding a duplicate code sends a warning", {
  new_code <- "gender"
  new_description <- "new code description"
  codes_df_test <- readRDS("data/qcoder_codes_my_qcoder_project.rds")
  expect_warning( add_code(codes_df_test, new_code, new_description,
           paste0(tempdir(),"/codes_test.rds")))
  unlink(paste0(tempdir(), "/codes_test.rds"))
})

test_that("Validating a default project folder structure works", {
  create_qcoder_project(project_name = "_my_qcoder_project")
  result <- validate_project("_my_qcoder_project")
  expect_equal(is.null(result), TRUE)
  unlink("./_my_qcoder_project", recursive=TRUE)
  expect_error(validate_project("_my_qcoder_project"))
})

test_that("Validating a default project data frame list works", {
  create_qcoder_project(project_name = "_my_qcoder_project", sample = TRUE)
  # Data was not imported so no rds files exist
  expect_error(validate_project_files("_my_qcoder_project"))
  import_project_data("_my_qcoder_project")
  result <- validate_project_files("_my_qcoder_project")
  unlink("./_my_qcoder_project", recursive=TRUE)
})

#Make a test for the zip function
test_that("Testing that zip function works", {
  create_qcoder_project(project_name = "my_qcoder_project", sample = TRUE)
 zip::zip(zipfile = paste0("QCoderProject-my_qcoder_project-",
                            Sys.Date(),".zip"), files = "my_qcoder_project", recurse = TRUE)
   expect_equal(file.exists(paste0(getwd(), "/QCoderProject-my_qcoder_project-" ,
                                   Sys.Date(),".zip")), TRUE)
  file.remove(paste0(getwd(),"/QCoderProject-my_qcoder_project-" ,
                     Sys.Date(),".zip"))
  unlink("./my_qcoder_project", recursive = TRUE)
}
          )
