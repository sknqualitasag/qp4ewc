test_that("testing function calculate_dailygain()", {

  s_input_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_flp.csv")
  s_start_flp_date <- 20050101
  s_end_flp_date <- 20211231
  b_log <- TRUE
  tbl_flp <- read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                 ps_start_flp_date = s_start_flp_date,
                                 ps_end_flp_date = s_end_flp_date,
                                 pb_log = b_log)


  l_constants <- get_constants()


  dailygain_f_expected <- 1.2118
  slaughterage_f <- 322.6667
  weaningage_f <- 170
  livewt_slaughter_f <- 398
  weaningwt_f <- 213
  dailygain_f <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f,
                                     pv_mean_weaningage = weaningage_f,
                                     pv_mean_livewt_atslaughter = livewt_slaughter_f,
                                     pv_mean_weaningwt = weaningwt_f,
                                     pb_log = b_log)
  expect_equal(dailygain_f, dailygain_f_expected)


})
