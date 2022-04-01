### #
### #
### #
### #   Purpose:   Function related to the calving steps
### #   started:   2022-03-30 (skn)
### #
### # ##################################################################### ###


s_input_file_calving <- "/Users/skn/muku_Ecoweigth/2022/data/zws_muku_gal/test_ANAUCHLMSIOB_zws_muku_gal.csv"
s_start_calving_date <- 20160101
s_end_calving_date <- 20211231
s_log <- TRUE

tbl_calving <- qp4ewc::read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                               ps_start_calving_date = s_start_calving_date,
                                               ps_end_calving_date = s_end_calving_date,
                                               pb_log = s_log)

#abortion_rate_primiparous <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
#                                                             ps_statement_firstlactation = TRUE,
#                                                             pb_log = s_log)
abortion_rate_primiparous <- 0.0036


#abortion_rate_multiparous <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
#                                                             ps_statement_firstlactation = FALSE,
#                                                             pb_log = s_log)
abortion_rate_multiparous <- 0.0012


s_sirebreed <- "LM"
s_prodsystew <- "2"
s_marketchannel <- "Natura-Beef"
s_path_directory2create <-"/Users/skn/muku_Ecoweigth/2022/work"

qp4ewc::create_directory_scenario(ps_sirebreed = s_sirebreed,
                          ps_prodsystew = s_prodsystew,
                          ps_marketchannel = s_marketchannel,
                          ps_path_directory2create = s_path_directory2create,
                          pb_log = s_log)


s_path2template_input_parameter_file <- file.path(s_path_directory2create,paste0(s_sirebreed,"_",s_prodsystew,"_",s_marketchannel),"INPUT02.TXT")
s_statement2search <- "Vector of probabilities of abortion for cows conceived in reproductive cycles 1 to LL"
s_value2update <- paste0(c(abortion_rate_primiparous, rep(abortion_rate_multiparous,9)),collapse = " ")

qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = s_path2template_input_parameter_file,
                            ps_statement2search = s_statement2search,
                            ps_value2update = s_value2update,
                            pb_log = s_log)



stillbirth_rate_easy_prim <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                               ps_statement_firstlactation = TRUE,
                                                               ps_statement_easycalving = TRUE,
                                                               pb_log = s_log)


calvingscore_prop_prim_LM_M_2 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = TRUE,
                                                                           ps_breed = "LM",
                                                                           ps_sex = "M",
                                                                           ps_calvingscore = 2,
                                                                           pb_log = s_log)


calvingdied24h_prop_prim_easy <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                            ps_statement_firstlactation = TRUE,
                                                                            ps_statement_easycalving = TRUE,
                                                                            pb_log = s_log)

