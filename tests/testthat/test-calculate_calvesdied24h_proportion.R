test_that("testing function calculate_calvesdied24h_proportion()", {

  # Read calving-file
  s_input_file_calving <- file.path(here::here(),"inst","extdata","test","test_zws_muku_gal.csv")
  s_start_calving_date <- 20150101
  s_end_calving_date <- 20220504
  b_log <- TRUE
  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_calving_date,
                                         ps_end_calving_date = s_end_calving_date,
                                         pb_log = b_log)


  # Proportion of calves died into 24 hours easy calving (calving scores 1 and 2)
  # for primiparous and
  calvingdied24h_prop_prim_easy_expected <- 0
  calvingdied24h_prop_prim_easy <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                      ps_statement_firstlactation = TRUE,
                                                                      ps_statement_easycalving = TRUE,
                                                                      pb_log = b_log)
  # for multiparous
  calvingdied24h_prop_multi_easy_expected <- 0
  calvingdied24h_prop_multi_easy <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                       ps_statement_firstlactation = FALSE,
                                                                       ps_statement_easycalving = TRUE,
                                                                       pb_log = b_log)
  value2update_calvingdied24hprop_easy_expected <- paste0(c(calvingdied24h_prop_prim_easy_expected, rep(calvingdied24h_prop_multi_easy_expected,9)),collapse = " ")
  value2update_calvingdied24hprop_easy <- paste0(c(calvingdied24h_prop_prim_easy, rep(calvingdied24h_prop_multi_easy,9)),collapse = " ")
  expect_equal(value2update_calvingdied24hprop_easy,value2update_calvingdied24hprop_easy_expected)


  # Proportion of calves died into 24 hours difficult calving (calving scores 3 and 4)
  # for primiparous
  calvingdied24h_prop_prim_difficult_expected <- 0
  calvingdied24h_prop_prim_difficult <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = TRUE,
                                                                           ps_statement_easycalving = FALSE,
                                                                           pb_log = b_log)
  # for multiparous
  calvingdied24h_prop_multi_difficult_expected <- 0
  calvingdied24h_prop_multi_difficult <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                            ps_statement_firstlactation = FALSE,
                                                                            ps_statement_easycalving = FALSE,
                                                                            pb_log = b_log)
  # Build a vector with 10 lactations for proportion of calves died into 24 hours and difficult calving
   value2update_calvingdied24hprop_difficult_expected <- paste0(c(calvingdied24h_prop_prim_difficult_expected, rep(calvingdied24h_prop_multi_difficult_expected,9)),collapse = " ")
   value2update_calvingdied24hprop_difficult <- paste0(c(calvingdied24h_prop_prim_difficult, rep(calvingdied24h_prop_multi_difficult,9)),collapse = " ")
   expect_equal(value2update_calvingdied24hprop_difficult,value2update_calvingdied24hprop_difficult_expected)



})
