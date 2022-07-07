test_that("testing function calculate_bull_liveweight()", {

  s_input_file_flp <- file.path(here::here(),"inst","extdata","ewbc","test","test_zws_muku_flp.csv")
  s_start_flp_date <- 20050101
  s_end_flp_date <- 20211231
  b_log <- TRUE
  s_input_file_ped <- file.path(here::here(),"inst","extdata","ewbc","test","test_pedigree.txt")
  s_sirebreed <- "LM"
  tbl_flp <- read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                 ps_start_flp_date = s_start_flp_date,
                                 ps_end_flp_date = s_end_flp_date,
                                 pb_log = b_log)
  tbl_ped <- read_file_input_ped(ps_input_file_ped = s_input_file_ped,
                                 pb_log = b_log)
  ### # Merge progeny-flp data and pedigree files
  tbl_merged_data <- tbl_flp %>% dplyr::inner_join(tbl_ped, by = c("NakoTVD" = "TVDid"))



  bull_mature_weight_expected <- 1060
  bull_mature_weight <- calculate_bull_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                                  ps_sirebreed = s_sirebreed,
                                                  pb_log = b_log)
  expect_equal(bull_mature_weight, bull_mature_weight_expected)



})
