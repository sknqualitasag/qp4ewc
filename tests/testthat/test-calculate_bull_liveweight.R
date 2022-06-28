test_that("testing function calculate_bull_liveweight()", {

  s_input_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_flp.csv")
  s_start_flp_date <- 20050101
  s_end_flp_date <- 20211231
  b_log <- TRUE
  tbl_flp <- read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                 ps_start_flp_date = s_start_flp_date,
                                 ps_end_flp_date = s_end_flp_date,
                                 pb_log = b_log)


  bull_mature_weight_expected <- 1060
  bull_mature_weight <- calculate_bull_liveweight(ps_input_flp_tibble = tbl_flp,
                                                  pb_log = b_log)
  expect_equal(bull_mature_weight, bull_mature_weight_expected)



})
