test_that("testing function read_price_conf_fat()", {

  # Read expected file
  s_output_matrix_cowprice <- file.path(here::here(),"inst","extdata","ewbc","test","test_mat_coeffprice_cow.csv")
  tbl_coeffprice_cow <- readr::read_delim(file = s_output_matrix_cowprice, delim = " ", col_names = FALSE)
  tbl_coeffprice_cow <- tbl_coeffprice_cow %>% dplyr::select(-c(1))
  tbl_coeffprice_cow <- tbl_coeffprice_cow[-1,]
  mat_coeffprice_cow_expected <- matrix(as.numeric(as.matrix(tbl_coeffprice_cow)), ncol = ncol(tbl_coeffprice_cow))


  # Parameters to call the fonction
  s_input_file_price_cow <- file.path(here::here(),"inst","extdata","ewbc","cow_price.txt")
  b_log <- TRUE
  mat_coeffprice_cow <- read_price_conf_fat(ps_input_file_price = s_input_file_price_cow,
                                            pb_log = b_log)
  expect_equal(mat_coeffprice_cow, mat_coeffprice_cow_expected)


})
