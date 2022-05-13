test_that("testing function calculate_extrapolated_weaningweight()", {

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


  l_constants <- get_constants()


  weight_300d_f_expected <- 370.53
  weaningage_f <- 170
  dailygain_f <- 1.2118
  weaningwt_f <- 213
  weight_300d_f <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                        pv_daily_gain = dailygain_f,
                                                        pv_mean_weaningwt = weaningwt_f,
                                                        pv_t_days = 300,
                                                        pb_log = b_log)
  expect_equal(weight_300d_f, weight_300d_f_expected)


})
