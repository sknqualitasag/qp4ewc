### #
### #
### #
### #   Purpose:   Function related to the pre-processing steps for each scenario to run ECOWEIGHT
### #   started:   2022-03-04 (skn)
### #
### # ##################################################################### ###



s_sirebreed <- "LM"
s_prodsystew <- "2"
s_marketchannel <- "Natura-Beef"
s_path_directory2create <-"/Users/skn/muku_Ecoweigth/2022/work"
s_input_file_literature <- file.path(here::here(),"inst","extdata","input_literature.txt")
s_input_file_calving_statement <- file.path(here::here(),"inst","extdata","input_calving_statement.txt")
s_input_file_calving <- "/Users/skn/muku_Ecoweigth/2022/data/zws_muku_gal/test_ANAUCHLMSIOB_zws_muku_gal.csv"
s_start_calving_date <- 20150101
s_end_calving_date <- 20211231
s_log <- TRUE
plogger = NULL

qp4ewc::pre_process_ew_input(ps_sirebreed = s_sirebreed,
                             ps_prodsystew = s_prodsystew,
                             ps_marketchannel = s_marketchannel,
                             ps_path_directory2create = s_path_directory2create,
                             ps_input_file_literature = s_input_file_literature,
                             ps_input_file_calving_statement = s_input_file_calving_statement,
                             ps_input_file_calving = s_input_file_calving,
                             ps_start_date = s_start_calving_date,
                             ps_end_date = s_end_calving_date,
                             pb_log = s_log)

