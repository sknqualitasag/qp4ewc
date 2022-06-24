test_that("testing function calculate_mean_birthweight()", {

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


  female_bw_expected <- 36.3333
  female_bw <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                          ps_sex = "F",
                                          ps_marketing_channel = l_constants$value_NaturaBeef,
                                          pb_log = b_log)
  expect_equal(female_bw, female_bw_expected)


  male_bw_expected <- 37
  male_bw <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                        ps_sex = "M",
                                        ps_marketing_channel = l_constants$value_SwissPrimBeef,
                                        pb_log = b_log)
  expect_equal(male_bw, male_bw_expected)


})
