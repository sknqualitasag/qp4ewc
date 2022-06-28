test_that("testing function calculate_mean_slaughterage()", {

  s_input_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_flp.csv")
  s_start_flp_date <- 20050101
  s_end_flp_date <- 20211231
  b_log <- TRUE
  s_prodsystew <- "2"
  tbl_flp <- read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                 ps_start_flp_date = s_start_flp_date,
                                 ps_end_flp_date = s_end_flp_date,
                                 pb_log = b_log)


  l_constants <- get_constants()


  slaughterage_f_expected <- 322.6667
  slaughterage_f <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_flp,
                                                ps_sex = "F",
                                                ps_marketing_channel = l_constants$value_NaturaBeef,
                                                ps_prodsystew = s_prodsystew,
                                                pb_log = b_log)
  expect_equal(slaughterage_f, slaughterage_f_expected)


})
