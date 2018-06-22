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
          "documents/CoC_Example4_MU.txt", "units/units.csv") )), 0)
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


