context("Parse coded documents")

data_path <-  "data/"
doctxt1 <- readr::read_file(paste0(data_path, "CoC_Example1_MU_nestingcheck.txt"))
doctxt2 <- readr::read_file(paste0(data_path, "CoC_Example2_MU.txt"))
doctxt3 <- readr::read_file(paste0(data_path, "CoC_Example3_MU.txt"))
doctxt4 <- readr::read_file(paste0(data_path, "CoC_Example2_MU_unbalanced_tags.txt"))
doctxt5 <- readr::read_file(paste0(data_path, "CoC_Example2_MU_missing_close_curly.txt"))
doctxt6 <- readr::read_file(paste0(data_path, "CoC_Example2_MU_missinghashtag.txt"))
getwd()
test_that("error_check passes a correct document without nested codes.", {
  expect_null(error_check(doctxt2))
})

test_that("error_check passes a correct document with nested codes.", {
  expect_null(error_check(doctxt1))
})

test_that("error_check handles a document with no codes.",  {

  expect_null(error_check(doctxt3))
})

test_that("error_check returns warnings for unmatched QCODE tags.", {
  expect_warning(error_check(doctxt4))

})

test_that("error_check returns warnings for missing #.", {
  expect_warning(error_check(doctxt6))

})
test_that("error_check returns warnings for missing }.", {
  expect_warning(error_check(doctxt5))

})

test_that("discovered codes are added to the existing codes data frame correctly", {

  add_discovered_code(codes_list = c("new_tag", "gender"),
                      code_data_frame = readRDS("./data/qcoder_codes_my_qcoder_project.rds"),
                      save_path = paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds"))
  # The function should have saved the data frame to the save_path folder
  input <- readRDS(paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds"))

  expect_equal(as.character(dplyr::pull(input,"code")[8]), "new_tag")
  expect_equal(as.character(dplyr::pull(input,"code_id")[8]), "8")
  unlink(paste0(tempdir(), "/qcoder_codes_my_qcoder_project.rds"))
})
