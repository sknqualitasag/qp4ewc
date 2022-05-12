test_that("testing function calculate_abortion_rate()", {

  s_input_file_calving <- file.path(here::here(),"inst","extdata","test","test_zws_muku_gal.csv")
  s_start_calving_date <- 20150101
  s_end_calving_date <- 20220504
  b_log <- TRUE
  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_calving_date,
                                         ps_end_calving_date = s_end_calving_date,
                                         pb_log = b_log)


  # Abortion rate for primiparous
  abortrate_prim_expected <- 0.2
  abortrate_prim <- calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                            ps_statement_firstlactation = TRUE,
                                            pb_log = b_log)
  expect_equal(abortrate_prim,
               abortrate_prim_expected)


  # Abortion rate for multiparous
  abortrate_multi_expected <- 0.2
  abortrate_multi <- calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                             ps_statement_firstlactation = FALSE,
                                             pb_log = b_log)
  expect_equal(abortrate_multi,
               abortrate_multi_expected)


  # Build a vector with 10 lactations for abortion rate
  value2update_abortrate_expected <- paste0(c(abortrate_prim_expected, rep(abortrate_multi_expected,9)),collapse = " ")
  value2update_abortrate <- paste0(c(abortrate_prim, rep(abortrate_multi,9)),collapse = " ")
  expect_equal(value2update_abortrate, value2update_abortrate_expected)


})
