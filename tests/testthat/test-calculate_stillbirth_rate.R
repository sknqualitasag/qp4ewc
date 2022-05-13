test_that("testing function calculate_stillbirth_rate()", {

  # Read calving-file
  s_input_file_calving <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_gal.csv")
  s_start_calving_date <- 20150101
  s_end_calving_date <- 20220504
  b_log <- TRUE
  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_calving_date,
                                         ps_end_calving_date = s_end_calving_date,
                                         pb_log = b_log)


  # stillbirth rate for easy calving (calving scores 1 and 2)
  # for primiparous
  stillbirthrate_prim_easy_expected <- 0.6667
  stillbirthrate_prim_easy <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                        ps_statement_firstlactation = TRUE,
                                                        ps_statement_easycalving = TRUE,
                                                        pb_log = b_log)
  # for multiparous
  stillbirthrate_multi_easy_expected <- 0.3333
  stillbirthrate_multi_easy <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                         ps_statement_firstlactation = FALSE,
                                                         ps_statement_easycalving = TRUE,
                                                         pb_log = b_log)
  # Build a vector with 10 lactations for stillbirth rate
  value2update_stillbirthrate_easy_expected <- paste0(c(stillbirthrate_prim_easy_expected, rep(stillbirthrate_multi_easy_expected,9)),collapse = " ")
  value2update_stillbirthrate_easy <- paste0(c(stillbirthrate_prim_easy, rep(stillbirthrate_multi_easy,9)),collapse = " ")
  expect_equal(value2update_stillbirthrate_easy, value2update_stillbirthrate_easy_expected)


  # stillbirth rate for difficult calving (calving scores 3 and 4)
  # for primiparous
  stillbirthrate_prim_difficult_expected <- 1
  stillbirthrate_prim_difficult <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                             ps_statement_firstlactation = TRUE,
                                                             ps_statement_easycalving = FALSE,
                                                             pb_log = b_log)
  # for multiparous
  stillbirthrate_multi_difficult_expected <- 0.5
  stillbirthrate_multi_difficult <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                              ps_statement_firstlactation = FALSE,
                                                              ps_statement_easycalving = FALSE,
                                                              pb_log = b_log)
  value2update_stillbirthrate_difficult_expected <- paste0(c(stillbirthrate_prim_difficult_expected, rep(stillbirthrate_multi_difficult_expected,9)),collapse = " ")
  value2update_stillbirthrate_difficult <- paste0(c(stillbirthrate_prim_difficult, rep(stillbirthrate_multi_difficult,9)),collapse = " ")
  expect_equal(value2update_stillbirthrate_difficult, value2update_stillbirthrate_difficult_expected)


})
