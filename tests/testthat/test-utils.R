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
  expect_equal(files, c("codes/codes.csv",
          "documents/CoC_Example1_mod_MU.txt", "documents/CoC_Example1_MU.txt",
          "documents/CoC_Example2_MU.txt", "documents/CoC_Example3_MU.txt",
          "documents/CoC_Example4_MU.txt", "units/units.csv"))
  expect_equal(dirs, c("./project2", "./project2/codes",
                       "./project2/data_frames", "./project2/documents",
                       "./project2/images", "./project2/media",
                       "./project2/memos", "./project2/misc",
                       "./project2/units"))
  unlink("./project2", recursive = TRUE)
  setwd(basewd)
})

