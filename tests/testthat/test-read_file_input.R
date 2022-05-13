test_that("testing function read_file_input()", {

  s_input_file_progeny_flp_statement <- file.path(here::here(),"inst","extdata","ewbc","input_flp_statement.txt")
  b_log <- TRUE


  tbl_input_statement_flp_expected <- readr::read_delim(file = s_input_file_progeny_flp_statement, delim = ";")
  tbl_input_statement_flp <- read_file_input(ps_input_file = s_input_file_progeny_flp_statement,
                                             pb_log = b_log)
  expect_equal(tbl_input_statement_flp, tbl_input_statement_flp_expected)


})
