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
s_log <- TRUE
s_input_file_literature <- file.path(here::here(),"inst","extdata","input_literature.txt")

qp4ewc::create_directory_scenario(ps_sirebreed = s_sirebreed,
                          ps_prodsystew = s_prodsystew,
                          ps_marketchannel = s_marketchannel,
                          ps_path_directory2create = s_path_directory2create,
                          pb_log = s_log)


qp4ewc::read_file_input_literature(ps_input_file_literature,
                           pb_log = s_log,
                           plogger = logger)


qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file,
                            ps_statement2search,
                            ps_value2update,
                            pb_log = s_log,
                            plogger = logger)
