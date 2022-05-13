test_that("Testing function calculate_calvesdiedafter24h_proportion()", {

  # Read calving-file
  s_input_file_calving <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_gal.csv")
  s_start_calving_date <- 20150101
  s_end_calving_date <- 20220504
  b_log <- TRUE
  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_calving_date,
                                         ps_end_calving_date = s_end_calving_date,
                                         pb_log = b_log)


  calf_loss_expected <- 0
  calf_loss <- calculate_calvesdiedafter24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                       pb_log = b_log)
  expect_equal(calf_loss,calf_loss_expected)


})
