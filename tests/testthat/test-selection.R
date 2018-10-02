context("testing selection functions")

test_text <- "This is a test. This sentence is selected."
selected_text <- "This sentence"
codes <- c("harrassment", "new_code")

replacement_text <- add_codes_to_selection(selection=selected_text, codes=codes)

output <- "(QCODE)This sentence(/QCODE){#harrassment,#new_code}"
output2 <- "This is a test. (QCODE)This sentence(/QCODE){#harrassment,#new_code} is selected."



#test_that("checking selection replacement routines",

#expect_equal(add_codes_to_selection(selection = selected_text, codes= codes), output),
#expect_equal(replace_selection(test_text, selected_text, replacement_text), output2)
#)
