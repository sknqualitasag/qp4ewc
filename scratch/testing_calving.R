### #
### #
### #
### #   Purpose:   Function related to the calving steps
### #   started:   2022-03-30 (skn)
### #
### # ##################################################################### ###

library(dplyr)

s_input_file_calving <- "/Users/skn/muku_Ecoweigth/2022/data/zws_muku_gal/test_zws_muku_gal.csv"
s_start_calving_date <- 20160101
s_end_calving_date <- 20211231
s_log <- TRUE

tbl_calving <- qp4ewc::read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                               ps_start_calving_date = s_start_calving_date,
                                               ps_end_calving_date = s_end_calving_date,
                                               pb_log = s_log)


abortion_rate <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                                 ps_statement_firstlactation = FALSE,
                                                 pb_log = s_log)


