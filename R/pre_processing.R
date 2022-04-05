### #
### #
### #
### #   Purpose:   Function related to the pre-processing steps
### #   started:   2022-03-09 (skn)
### #
### # ##################################################################### ###


#' @title Pre-processing the input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files.
#'
#' @param ps_sirebreed sire breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_literature path to file with input coming from literature for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_calving_date setting the start of the calving date to filter in the calving data
#' @param ps_end_calving_date setting the end of the calving date to filter in the calving data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ew_input
pre_process_ew_input <- function(ps_sirebreed,
                                 ps_prodsystew,
                                 ps_marketchannel,
                                 ps_path_directory2create,
                                 ps_input_file_literature,
                                 ps_input_file_calving_statement,
                                 ps_input_file_calving,
                                 ps_start_calving_date,
                                 ps_end_calving_date,
                                 pb_log = FALSE,
                                 plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_literature: ', ps_input_file_literature, '\n',
                           ' * ps_input_file_calving_statement: ',ps_input_file_calving_statement, '\n',
                           ' * ps_input_file_calving: ', ps_input_file_calving, '\n',
                           ' * ps_start_calving_date: ',ps_start_calving_date,'\n',
                           ' * ps_end_calving_date: ',ps_end_calving_date,'\n'))
  }


  ### # Create directory per scenario with input-parameter-file for ECOWEIGHT
  qp4ewc::create_directory_scenario(ps_sirebreed,
                                    ps_prodsystew,
                                    ps_marketchannel,
                                    ps_path_directory2create,
                                    pb_log = TRUE,
                                    plogger = lgr)


  ### # ------------------------------------------------------------------------
  ### # Literature
  ### # ------------------------------------------------------------------------
  ### # Read file with input from literature research for input-parameter-file of ECOWEIGHT
  tbl_input_literature <- qp4ewc::read_file_input(ps_input_file_literature,
                                                  pb_log = TRUE,
                                                  plogger = lgr)
  ### # Update input-parameter-file coming from literature of ECOWEIGHT
  for(l in 1:nrow(tbl_input_literature)){
    qp4ewc_log_info(lgr, 'pre_process_ew_input',
                    paste0('Updating parameter with input coming from the literature file:\n * line number l: ', l, '\n'))
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_literature[l,1]),
                                        ps_statement2search = tbl_input_literature[l,2],
                                        ps_value2update = tbl_input_literature[l,4],
                                        pb_log = TRUE,
                                        plogger = lgr)
  }

  ### # ------------------------------------------------------------------------
  ### # Calving
  ### # ------------------------------------------------------------------------
  ### # Pre-processing the calving data for input-parameter-file of ECOWEIGHT
  qp4ewc::pre_process_ew_input_calving(ps_sirebreed,
                                       ps_prodsystew,
                                       ps_marketchannel,
                                       ps_path_directory2create,
                                       ps_input_file_literature,
                                       ps_input_file_calving_statement,
                                       ps_input_file_calving,
                                       ps_start_calving_date,
                                       ps_end_calving_date,
                                       pb_log = TRUE,
                                       plogger = lgr)



  ### # ------------------------------------------------------------------------
  ### # Carcass
  ### # ------------------------------------------------------------------------


}



#' @title Pre-processing the calving data for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on calving data.
#'
#' @param ps_sirebreed sire breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_literature path to file with input coming from literature for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_calving_date setting the start of the calving date to filter in the calving data
#' @param ps_end_calving_date setting the end of the calving date to filter in the calving data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ew_input_calving
pre_process_ew_input_calving <- function(ps_sirebreed,
                                         ps_prodsystew,
                                         ps_marketchannel,
                                         ps_path_directory2create,
                                         ps_input_file_literature,
                                         ps_input_file_calving_statement,
                                         ps_input_file_calving,
                                         ps_start_calving_date,
                                         ps_end_calving_date,
                                         pb_log = FALSE,
                                         plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input_calving.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input_calving',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_literature: ', ps_input_file_literature, '\n',
                           ' * ps_input_file_calving_statement: ',ps_input_file_calving_statement, '\n',
                           ' * ps_input_file_calving: ', ps_input_file_calving, '\n',
                           ' * ps_start_calving_date: ',ps_start_calving_date,'\n',
                           ' * ps_end_calving_date: ',ps_end_calving_date,'\n'))
  }


  ### # Read file with statement based on calving for input-parameter-file of ECOWEIGHT
  tbl_input_statement_calving <- qp4ewc::read_file_input(ps_input_file_calving_statement,
                                                         pb_log = TRUE,
                                                         plogger = lgr)


  ### # Read file with statement based on calving for input-parameter-file of ECOWEIGHT
  tbl_calving <- qp4ewc::read_file_input_calving(ps_input_file_calving,
                                                 ps_start_calving_date,
                                                 ps_end_calving_date,
                                                 pb_log = TRUE,
                                                 plogger = lgr)


  ### # Update statement-calving-input from the data by calculating abortion rate
  abortrate_prim <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                                    ps_statement_firstlactation = TRUE,
                                                    pb_log = TRUE,
                                                    plogger = lgr)
  abortrate_multi <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                                     ps_statement_firstlactation = FALSE,
                                                     pb_log = TRUE,
                                                     plogger = lgr)
  value2update_abortrate <- paste0(c(abortrate_prim, rep(abortrate_multi,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[1,1]),
                                      ps_statement2search = tbl_input_statement_calving[1,2],
                                      ps_value2update = value2update_abortrate,
                                      pb_log = TRUE,
                                      plogger = lgr)


  ### # Update statement-calving-input from the data by calculating stillbirth rate
  stillbirthrate_prim_easy <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                ps_statement_firstlactation = TRUE,
                                                                ps_statement_easycalving = TRUE,
                                                                pb_log = FALSE,
                                                                plogger = lgr)
  stillbirthrate_multi_easy <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                 ps_statement_firstlactation = FALSE,
                                                                 ps_statement_easycalving = TRUE,
                                                                 pb_log = FALSE,
                                                                 plogger = lgr)
  value2update_stillbirthrate_easy <- paste0(c(stillbirthrate_prim_easy, rep(stillbirthrate_multi_easy,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[2,1]),
                                      ps_statement2search = tbl_input_statement_calving[2,2],
                                      ps_value2update = value2update_stillbirthrate_easy,
                                      pb_log = TRUE,
                                      plogger = lgr)


  stillbirthrate_prim_difficult <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                     ps_statement_firstlactation = TRUE,
                                                                     ps_statement_easycalving = FALSE,
                                                                     pb_log = FALSE,
                                                                     plogger = lgr)
  stillbirthrate_multi_difficult <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                      ps_statement_firstlactation = FALSE,
                                                                      ps_statement_easycalving = FALSE,
                                                                      pb_log = FALSE,
                                                                      plogger = lgr)
  value2update_stillbirthrate_difficult <- paste0(c(stillbirthrate_prim_difficult, rep(stillbirthrate_multi_difficult,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[3,1]),
                                      ps_statement2search = tbl_input_statement_calving[3,2],
                                      ps_value2update = value2update_stillbirthrate_difficult,
                                      pb_log = TRUE,
                                      plogger = lgr)


  ### # Update calving score parameter inputs
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[4,1]),
                                      ps_statement2search = tbl_input_statement_calving[4,2],
                                      ps_value2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[5,1]),
                                      ps_statement2search = tbl_input_statement_calving[5,2],
                                      ps_value2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)


  ### # Update calving score proportions
  calvingscore_prop_prim_F_2 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = TRUE,
                                                                          ps_breed = ps_sirebreed,
                                                                          ps_sex = "F",
                                                                          ps_calvingscore = 2,
                                                                          pb_log = TRUE,
                                                                          plogger = lgr)
  calvingscore_prop_prim_F_3 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = TRUE,
                                                                          ps_breed = ps_sirebreed,
                                                                          ps_sex = "F",
                                                                          ps_calvingscore = 3,
                                                                          pb_log = TRUE,
                                                                          plogger = lgr)
  calvingscore_prop_prim_F_4 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = TRUE,
                                                                          ps_breed = ps_sirebreed,
                                                                          ps_sex = "F",
                                                                          ps_calvingscore = 4,
                                                                          pb_log = TRUE,
                                                                          plogger = lgr)
  calvingscore_prop_multi_F_2 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = FALSE,
                                                                           ps_breed = ps_sirebreed,
                                                                           ps_sex = "F",
                                                                           ps_calvingscore = 2,
                                                                           pb_log = TRUE,
                                                                           plogger = lgr)
  calvingscore_prop_multi_F_3 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = FALSE,
                                                                           ps_breed = ps_sirebreed,
                                                                           ps_sex = "F",
                                                                           ps_calvingscore = 3,
                                                                           pb_log = TRUE,
                                                                           plogger = lgr)
  calvingscore_prop_multi_F_4 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = FALSE,
                                                                           ps_breed = ps_sirebreed,
                                                                           ps_sex = "F",
                                                                           ps_calvingscore = 4,
                                                                           pb_log = TRUE,
                                                                           plogger = lgr)
  calvingscore_prop_prim_M_2 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = TRUE,
                                                                          ps_breed = ps_sirebreed,
                                                                          ps_sex = "M",
                                                                          ps_calvingscore = 2,
                                                                          pb_log = TRUE,
                                                                          plogger = lgr)
  calvingscore_prop_prim_M_3 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = TRUE,
                                                                          ps_breed = ps_sirebreed,
                                                                          ps_sex = "M",
                                                                          ps_calvingscore = 3,
                                                                          pb_log = TRUE,
                                                                          plogger = lgr)
  calvingscore_prop_prim_M_4 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = TRUE,
                                                                          ps_breed = ps_sirebreed,
                                                                          ps_sex = "M",
                                                                          ps_calvingscore = 4,
                                                                          pb_log = TRUE,
                                                                          plogger = lgr)
  calvingscore_prop_multi_M_2 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = FALSE,
                                                                           ps_breed = ps_sirebreed,
                                                                           ps_sex = "M",
                                                                           ps_calvingscore = 2,
                                                                           pb_log = TRUE,
                                                                           plogger = lgr)
  calvingscore_prop_multi_M_3 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = FALSE,
                                                                           ps_breed = ps_sirebreed,
                                                                           ps_sex = "M",
                                                                           ps_calvingscore = 3,
                                                                           pb_log = TRUE,
                                                                           plogger = lgr)
  calvingscore_prop_multi_M_4 <- qp4ewc::calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = FALSE,
                                                                           ps_breed = ps_sirebreed,
                                                                           ps_sex = "M",
                                                                           ps_calvingscore = 4,
                                                                           pb_log = TRUE,
                                                                           plogger = lgr)
  value2update_calvingscoreprop_prim_F_2 <- paste0(c(calvingscore_prop_prim_F_2, rep(calvingscore_prop_multi_F_2,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[6,1]),
                                      ps_statement2search = tbl_input_statement_calving[6,2],
                                      ps_value2update = value2update_calvingscoreprop_prim_F_2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  value2update_calvingscoreprop_prim_F_3 <- paste0(c(calvingscore_prop_prim_F_3, rep(calvingscore_prop_multi_F_3,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[7,1]),
                                      ps_statement2search = tbl_input_statement_calving[7,2],
                                      ps_value2update = value2update_calvingscoreprop_prim_F_3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  value2update_calvingscoreprop_prim_F_4 <- paste0(c(calvingscore_prop_prim_F_4, rep(calvingscore_prop_multi_F_4,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[8,1]),
                                      ps_statement2search = tbl_input_statement_calving[8,2],
                                      ps_value2update = value2update_calvingscoreprop_prim_F_4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  value2update_calvingscoreprop_prim_M_2 <- paste0(c(calvingscore_prop_prim_M_2, rep(calvingscore_prop_multi_M_2,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[9,1]),
                                      ps_statement2search = tbl_input_statement_calving[9,2],
                                      ps_value2update = value2update_calvingscoreprop_prim_M_2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  value2update_calvingscoreprop_prim_M_3 <- paste0(c(calvingscore_prop_prim_M_3, rep(calvingscore_prop_multi_M_3,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[10,1]),
                                      ps_statement2search = tbl_input_statement_calving[10,2],
                                      ps_value2update = value2update_calvingscoreprop_prim_M_3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  value2update_calvingscoreprop_prim_M_4 <- paste0(c(calvingscore_prop_prim_M_4, rep(calvingscore_prop_multi_M_4,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[11,1]),
                                      ps_statement2search = tbl_input_statement_calving[11,2],
                                      ps_value2update = value2update_calvingscoreprop_prim_M_4,
                                      pb_log = TRUE,
                                      plogger = lgr)


  ### # Update proportion of calves died to 24 hours
  calvingdied24h_prop_prim_easy <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                              ps_statement_firstlactation = TRUE,
                                                                              ps_statement_easycalving = TRUE,
                                                                              pb_log = TRUE,
                                                                              plogger = lgr)
  calvingdied24h_prop_multi_easy <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                               ps_statement_firstlactation = FALSE,
                                                                               ps_statement_easycalving = TRUE,
                                                                               pb_log = TRUE,
                                                                               plogger = lgr)
  value2update_calvingdied24hprop_easy <- paste0(c(calvingdied24h_prop_prim_easy, rep(calvingdied24h_prop_multi_easy,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[13,1]),
                                      ps_statement2search = tbl_input_statement_calving[13,2],
                                      ps_value2update = value2update_calvingdied24hprop_easy,
                                      pb_log = TRUE,
                                      plogger = lgr)
  calvingdied24h_prop_prim_difficult <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                                   ps_statement_firstlactation = TRUE,
                                                                                   ps_statement_easycalving = FALSE,
                                                                                   pb_log = TRUE,
                                                                                   plogger = lgr)
  calvingdied24h_prop_multi_difficult <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                                    ps_statement_firstlactation = FALSE,
                                                                                    ps_statement_easycalving = FALSE,
                                                                                    pb_log = TRUE,
                                                                                    plogger = lgr)
  value2update_calvingdied24hprop_difficult <- paste0(c(calvingdied24h_prop_prim_difficult, rep(calvingdied24h_prop_multi_difficult,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[12,1]),
                                      ps_statement2search = tbl_input_statement_calving[12,2],
                                      ps_value2update = value2update_calvingdied24hprop_difficult,
                                      pb_log = TRUE,
                                      plogger = lgr)

}
