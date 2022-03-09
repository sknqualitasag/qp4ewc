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

#qp4ewc::create_directory_scenario(ps_sirebreed = s_sirebreed,
#                          ps_prodsystew = s_prodsystew,
#                          ps_marketchannel = s_marketchannel,
#                          ps_path_directory2create = s_path_directory2create,
#                          pb_log = s_log)
#
#
#tbl_input_literature <- qp4ewc::read_file_input_literature(ps_input_file_literature = s_input_file_literature,
#                           pb_log = s_log)
#
#s_path2template_input_parameter_file <- file.path(s_path_directory2create,paste0(s_sirebreed,"_",s_prodsystew,"_",s_marketchannel),tbl_input[1,1])
#s_statement2search <- tbl_input[1,2]
#s_value2update <- tbl_input[1,4]
#qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = s_path2template_input_parameter_file,
#                            ps_statement2search = s_statement2search,
#                            ps_value2update = s_value2update,
#                            pb_log = s_log)
#
#for(l in 1:nrow(tbl_input_literature)){
#
#
#  s_path2template_input_literatur_parameter_file <- file.path(s_path_directory2create,paste0(s_sirebreed,"_",s_prodsystew,"_",s_marketchannel),tbl_input_literature[l,1])
#  s_statement2search_literatur <- tbl_input_literature[l,2]
#  s_value2update_literatur <- tbl_input_literature[l,4]
#
#  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = s_path2template_input_literatur_parameter_file,
#                                      ps_statement2search = s_statement2search_literatur,
#                                      ps_value2update = s_value2update_literatur,
#                                      pb_log = s_log)
#
#}


qp4ewc::pre_process_ew_input(ps_sirebreed = s_sirebreed,
                             ps_prodsystew = s_prodsystew,
                             ps_marketchannel = s_marketchannel,
                             ps_path_directory2create = s_path_directory2create,
                             ps_input_file_literature = s_input_file_literature,
                             pb_log = s_log)

