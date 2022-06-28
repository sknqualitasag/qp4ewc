test_that("testing function calculate_cow_liveweight()", {

  s_input_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_flp.csv")
  s_start_flp_date <- 20050101
  s_end_flp_date <- 20211231
  b_log <- TRUE
  tbl_flp <- read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                 ps_start_flp_date = s_start_flp_date,
                                 ps_end_flp_date = s_end_flp_date,
                                 pb_log = b_log)


  second_calving_wt_expected <- 671
  second_calving_wt <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_flp,
                                                ps_first_calvingweight = FALSE,
                                                ps_second_calvingweight = TRUE,
                                                pb_log = b_log)
  expect_equal(second_calving_wt, second_calving_wt_expected)


  mature_weight_cow_expected <- 606
  mature_weight_cow <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_flp,
                                                ps_first_calvingweight = FALSE,
                                                ps_second_calvingweight = FALSE,
                                                pb_log = b_log)
  expect_equal(mature_weight_cow, mature_weight_cow_expected)



})
