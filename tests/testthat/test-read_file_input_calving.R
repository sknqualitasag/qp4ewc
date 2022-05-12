test_that("testing function read_file_input_calving()", {

  s_expected_file_calving <- file.path(here::here(),"inst","extdata","test","test_tbl_calving.csv")
  tbl_calving_expected <- readr::read_delim(file = s_expected_file_calving, delim = ";")


  s_input_file_calving <- file.path(here::here(),"inst","extdata","test","test_zws_muku_gal.csv")
  s_start_calving_date <- 20150101
  s_end_calving_date <- 20220504
  b_log <- TRUE
  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_calving_date,
                                         ps_end_calving_date = s_end_calving_date,
                                         pb_log = b_log)


  expect_equal(tbl_calving, tbl_calving_expected)

})
