### #
### #
### #
### #   Purpose:   Function related to the flp input file based on conformation and fat scores
### #   started:   2022-04-19 (skn)
### #
### # ##################################################################### ###


s_input_file_flp <- "/Users/skn/muku_Ecoweigth/2022/data/zws_muku_flp/test_ANAUCHLMSIOB_zws_muku_flp.csv"
s_start_flp_date <- 20160101
s_end_flp_date <- 20211231
s_sirebreed <- "LM"
s_log <- TRUE
s_input_file_flp_carcass_matrix_statement <- file.path(here::here(),"inst","extdata","input_flp_carcass_matrix_statement.txt")


tbl_input_statement_flp_carcass <- qp4ewc::read_file_input(ps_input_file = s_input_file_flp_carcass_matrix_statement,
                                                           pb_log = s_log,
                                                           plogger = NULL)



tbl_flp <- qp4ewc::read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                       ps_start_flp_date = s_start_flp_date,
                                       ps_end_flp_date = s_end_flp_date,
                                       ps_sirebreed = s_sirebreed,
                                       pb_log = s_log,
                                       plogger = NULL)


freq_mat_cow <- qp4ewc::build_freq_conf_fat(ps_input_flp_tibble = tbl_flp,
                                                 ps_sex = "F",
                                                 ps_marketing_channel = NULL,
                                                 ps_flag_cow = TRUE,
                                                 pb_log = s_log,
                                                 plogger = NULL)

paste0(as.character(round(freq_mat_cow[1,],4)),collapse = " ")

s_input_file_price_cow <- file.path(here::here(),"inst","extdata","cow_price.txt")

mat_coeffprice <- qp4ewc::read_price_conf_fat(ps_input_file_price = s_input_file_price_cow,
                                              pb_log = s_log,
                                              plogger = NULL)


