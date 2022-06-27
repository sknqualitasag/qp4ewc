test_that("testing function calculate_calvingscore_proportion()", {

  # Read calving-file
  s_input_file_calving <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_gal.csv")
  s_start_calving_date <- 20150101
  s_end_calving_date <- 20220504
  b_log <- TRUE
  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_calving_date,
                                         ps_end_calving_date = s_end_calving_date,
                                         pb_log = b_log)


  s_sirebreed <- "LM"
  s_dambreed <- "LM"
  #  Proportion for calving score 2 for females
  calvingscore_prop_prim_F_2_expected <- 0.5
  calvingscore_prop_prim_F_2 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = "F",
                                                                  ps_calvingscore = 2,
                                                                  ps_sirebreed = s_sirebreed,
                                                                  ps_dambreed = s_dambreed,
                                                                  pb_log = b_log)
  calvingscore_prop_multi_F_2_expected <- 0
  calvingscore_prop_multi_F_2 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = "F",
                                                                   ps_calvingscore = 2,
                                                                   ps_sirebreed = s_sirebreed,
                                                                   ps_dambreed = s_dambreed,
                                                                   pb_log = b_log)
  # Build a vector with 10 lactations
  value2update_calvingscoreprop_prim_F_2_expected <- paste0(c(calvingscore_prop_prim_F_2_expected, rep(calvingscore_prop_multi_F_2_expected,9)),collapse = " ")
  value2update_calvingscoreprop_prim_F_2 <- paste0(c(calvingscore_prop_prim_F_2, rep(calvingscore_prop_multi_F_2,9)),collapse = " ")
  expect_equal(value2update_calvingscoreprop_prim_F_2, value2update_calvingscoreprop_prim_F_2_expected)



  #  Proportion for calving score 3 for males
  calvingscore_prop_prim_M_3_expected <- 0.3333
  calvingscore_prop_prim_M_3 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = "M",
                                                                  ps_calvingscore = 3,
                                                                  ps_sirebreed = s_sirebreed,
                                                                  ps_dambreed = s_dambreed,
                                                                  pb_log = b_log)
  calvingscore_prop_multi_M_3_expected <- 0
  calvingscore_prop_multi_M_3 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = "M",
                                                                   ps_calvingscore = 3,
                                                                   ps_sirebreed = s_sirebreed,
                                                                   ps_dambreed = s_dambreed,
                                                                   pb_log = b_log)
  # Build a vector with 10 lactations for calving score 3 proportion for males
  value2update_calvingscoreprop_prim_M_3_expected <- paste0(c(calvingscore_prop_prim_M_3_expected, rep(calvingscore_prop_multi_M_3_expected,9)),collapse = " ")
  value2update_calvingscoreprop_prim_M_3 <- paste0(c(calvingscore_prop_prim_M_3, rep(calvingscore_prop_multi_M_3,9)),collapse = " ")
  expect_equal(value2update_calvingscoreprop_prim_M_3, value2update_calvingscoreprop_prim_M_3_expected)


})
