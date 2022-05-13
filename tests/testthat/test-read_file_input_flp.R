test_that("testing function read_file_input_flp()", {

  s_expected_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_tbl_flp.csv")
  tbl_flp_expected <- readr::read_delim(file = s_expected_file_flp, delim = ";")


  s_input_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_flp.csv")
  s_start_flp_date <- 20050101
  s_end_flp_date <- 20211231
  s_sirebreed <- "LM"
  b_log <- TRUE
  tbl_flp <- read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                 ps_start_flp_date = s_start_flp_date,
                                 ps_end_flp_date = s_end_flp_date,
                                 ps_sirebreed = s_sirebreed,
                                 pb_log = b_log)


  expect_equal(tbl_flp, tbl_flp_expected)


})
