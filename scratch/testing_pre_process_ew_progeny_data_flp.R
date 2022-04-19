### #
### #
### #
### #   Purpose:   Function testing pre_process_ew_progeny_data_flp
### #   started:   2022-04-09 (skn)
### #
### # ##################################################################### ###

s_sirebreed <- "LM"
s_prodsystew <- "2"
s_marketchannel <- "Natura-Beef"
s_path_directory2create <-"/Users/skn/muku_Ecoweigth/2022/work"
s_input_file_progeny_flp_statement <- file.path(here::here(),"inst","extdata","input_flp_statement.txt")
s_input_file_flp <- "/Users/skn/muku_Ecoweigth/2022/data/zws_muku_flp/test_ANAUCHLMSIOB_zws_muku_flp.csv"
s_start_flp_date <- 20160101
s_end_flp_date <- 20211231
s_log <- TRUE


qp4ewc::pre_process_ew_input_progeny_data_flp(ps_sirebreed = s_sirebreed,
                                              ps_prodsystew = s_prodsystew,
                                              ps_marketchannel = s_marketchannel,
                                              ps_path_directory2create = s_path_directory2create,
                                              ps_input_file_progeny_flp_statement = s_input_file_progeny_flp_statement,
                                              ps_input_file_flp = s_input_file_flp,
                                              ps_start_flp_date = s_start_flp_date,
                                              ps_end_flp_date = s_end_flp_date,
                                              pb_log = s_log,
                                              plogger = NULL)

