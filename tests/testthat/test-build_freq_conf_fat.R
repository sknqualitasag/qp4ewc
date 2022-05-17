test_that("testing function build_freq_conf_fat()", {

  # Read file expected
  s_output_matrix_freqmatcow <- file.path(here::here(),"inst","extdata","ewbc","test","test_mat_freqmat_cow.csv")
  tbl_freq_mat_cow <- readr::read_delim(file = s_output_matrix_freqmatcow, delim = " ", col_names = FALSE)
  tbl_freq_mat_cow <- tbl_freq_mat_cow %>% dplyr::select(-c(1))
  tbl_freq_mat_cow <- tbl_freq_mat_cow[-1,]
  freq_mat_cow_expected <- matrix(as.numeric(as.matrix(tbl_freq_mat_cow)), ncol = ncol(tbl_freq_mat_cow))


  # Parameter to call the function
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
  freq_mat_cow <- build_freq_conf_fat(ps_input_flp_tibble = tbl_flp,
                                      ps_sex = "F",
                                      ps_marketing_channel = NULL,
                                      ps_flag_cow = TRUE,
                                      pb_log = b_log)
  expect_equal(freq_mat_cow, freq_mat_cow_expected)


})
