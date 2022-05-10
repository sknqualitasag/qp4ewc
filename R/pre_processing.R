### #
### #
### #
### #   Purpose:   Function related to the pre-processing steps
### #   started:   2022-03-09 (skn)
### #
### # ##################################################################### ###


#' @title Pre-processing the input-parameter-file of ECOWEIGHT beef cattle
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
#' @param ps_start_date setting the start of the calving date to filter in the calving data
#' @param ps_end_date setting the end of the calving date to filter in the calving data
#' @param ps_input_file_progeny_flp_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp_carcass_matrix_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_price_cow path to file with price for cows
#' @param ps_input_file_price_bull path to file with price for bulls
#' @param ps_input_file_price_heifer path tho file with price for heifers
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ewbc_input
pre_process_ewbc_input <- function(ps_sirebreed,
                                 ps_prodsystew,
                                 ps_marketchannel,
                                 ps_path_directory2create,
                                 ps_input_file_literature,
                                 ps_input_file_calving_statement,
                                 ps_input_file_calving,
                                 ps_start_date,
                                 ps_end_date,
                                 ps_input_file_progeny_flp_statement,
                                 ps_input_file_flp,
                                 ps_input_file_flp_carcass_matrix_statement,
                                 ps_input_file_price_cow,
                                 ps_input_file_price_bull,
                                 ps_input_file_price_heifer,
                                 pb_log = FALSE,
                                 plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ewbc_input.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ewbc_input',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_literature: ', ps_input_file_literature, '\n',
                           ' * ps_input_file_calving_statement: ',ps_input_file_calving_statement, '\n',
                           ' * ps_input_file_calving: ', ps_input_file_calving, '\n',
                           ' * ps_start_date: ',ps_start_date,'\n',
                           ' * ps_end_date: ',ps_end_date,'\n'))
  }


  ### # Major step 1 ### #
  ### # Create directory per scenario with input-parameter-file for ECOWEIGHT
  qp4ewc::create_directory_scenario(ps_sirebreed,
                                    ps_prodsystew,
                                    ps_marketchannel,
                                    ps_path_directory2create,
                                    pb_log = TRUE,
                                    plogger = lgr)


  # ****************************************************************************
  ## ---- Literature ----
  # ****************************************************************************
  ### # Major step 2 ### #
  ### # Read file with input from literature research for input-parameter-file of ECOWEIGHT
  tbl_input_literature <- qp4ewc::read_file_input(ps_input_file_literature,
                                                  pb_log = TRUE,
                                                  plogger = lgr)
  ### # Update input-parameter-file coming from literature of ECOWEIGHT
  for(l in 1:nrow(tbl_input_literature)){
    qp4ewc_log_info(lgr, 'pre_process_ewbc_input',
                    paste0('Updating parameter with input coming from the literature file:\n * line number l: ', l, '\n'))
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_literature[l,1]),
                                        ps_statement2search = tbl_input_literature[l,2],
                                        ps_value2update = tbl_input_literature[l,4],
                                        pb_log = TRUE,
                                        plogger = lgr)
  }

  # ****************************************************************************
  ## ---- Calving ----
  # ****************************************************************************
  ### # Major step 3 ### #
  ### # Pre-processing the calving data for input-parameter-file of ECOWEIGHT
  qp4ewc::pre_process_ew_input_calving(ps_sirebreed,
                                       ps_prodsystew,
                                       ps_marketchannel,
                                       ps_path_directory2create,
                                       ps_input_file_calving_statement,
                                       ps_input_file_calving,
                                       ps_start_calving_date = ps_start_date,
                                       ps_end_calving_date = ps_end_date,
                                       pb_log = TRUE,
                                       plogger = lgr)


  qp4ewc_log_info(lgr, 'pre_process_ewbc_input',
                  paste0('Updating parameter with input coming from the calving file '))


  # ****************************************************************************
  ## ---- Progreny data about weighing and slaughter ----
  # ****************************************************************************
  ### # Major step 4 ### #
  qp4ewc::pre_process_ewbc_input_progeny_data_flp(ps_sirebreed,
                                                  ps_prodsystew,
                                                  ps_marketchannel,
                                                  ps_path_directory2create,
                                                  ps_input_file_progeny_flp_statement,
                                                  ps_input_file_flp,
                                                  ps_start_flp_date = ps_start_date,
                                                  ps_end_flp_date = ps_end_date,
                                                  pb_log = TRUE,
                                                  plogger = lgr)

  qp4ewc_log_info(lgr, 'pre_process_ewbc_input',
                  paste0('Updating parameter with input coming from the progeny data flp file '))



  # ****************************************************************************
  ## ---- Conformation & fat scores and price matrices ----
  # ****************************************************************************
  ### # Major step 5 ### #
  qp4ewc::pre_process_ew_input_carcass_data_flp(ps_sirebreed,
                                                ps_prodsystew,
                                                ps_marketchannel,
                                                ps_path_directory2create,
                                                ps_input_file_flp_carcass_matrix_statement,
                                                ps_input_file_flp,
                                                ps_start_flp_date = ps_start_date,
                                                ps_end_flp_date = ps_end_date,
                                                ps_input_file_price_cow,
                                                ps_input_file_price_bull,
                                                ps_input_file_price_heifer,
                                                pb_log = TRUE,
                                                plogger = lgr)

  qp4ewc_log_info(lgr, 'pre_process_ewbc_input',
                  paste0('Updating parameter with input coming from the conformation & fat scores, prices '))


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
                           ' * ps_input_file_calving_statement: ',ps_input_file_calving_statement, '\n',
                           ' * ps_input_file_calving: ', ps_input_file_calving, '\n',
                           ' * ps_start_calving_date: ',ps_start_calving_date,'\n',
                           ' * ps_end_calving_date: ',ps_end_calving_date,'\n'))
  }


  ### # Read file with statement based on calving for input-parameter-file of ECOWEIGHT
  tbl_input_statement_calving <- qp4ewc::read_file_input(ps_input_file_calving_statement,
                                                         pb_log = TRUE,
                                                         plogger = lgr)


  ### # Read file with calving data
  tbl_calving <- qp4ewc::read_file_input_calving(ps_input_file_calving,
                                                 ps_start_calving_date,
                                                 ps_end_calving_date,
                                                 pb_log = TRUE,
                                                 plogger = lgr)


  # Update statement-calving-input from the data by calculating abortion rate
  abortrate_prim <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                                    ps_statement_firstlactation = TRUE,
                                                    pb_log = TRUE,
                                                    plogger = lgr)
  # Check if abortrate_prim is zero. If it is the case, set a default value
  if(abortrate_prim == 0){
    abortrate_prim <- unlist(strsplit(tbl_input_statement_calving[1,4]$input_value, split = " "))[1]
  }
  abortrate_multi <- qp4ewc::calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                                     ps_statement_firstlactation = FALSE,
                                                     pb_log = TRUE,
                                                     plogger = lgr)
  # Check if abortrate_multi is zero. If it is the case, set a default value
  if(abortrate_multi == 0){
    abortrate_multi <- unlist(strsplit(tbl_input_statement_calving[1,4]$input_value, split = " "))[2]
  }
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
                                                                pb_log = TRUE,
                                                                plogger = lgr)
  # Check if stillbirthrate_prim_easy is zero. If it is the case, set a default value
  if(stillbirthrate_prim_easy == 0){
    stillbirthrate_prim_easy <- unlist(strsplit(tbl_input_statement_calving[2,4]$input_value, split = " "))[1]
  }
  stillbirthrate_multi_easy <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                 ps_statement_firstlactation = FALSE,
                                                                 ps_statement_easycalving = TRUE,
                                                                 pb_log = TRUE,
                                                                 plogger = lgr)
  # Check if stillbirthrate_multi_easy is zero. If it is the case, set a default value
  if(stillbirthrate_multi_easy == 0){
    stillbirthrate_multi_easy <- unlist(strsplit(tbl_input_statement_calving[2,4]$input_value, split = " "))[2]
  }
  value2update_stillbirthrate_easy <- paste0(c(stillbirthrate_prim_easy, rep(stillbirthrate_multi_easy,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[2,1]),
                                      ps_statement2search = tbl_input_statement_calving[2,2],
                                      ps_value2update = value2update_stillbirthrate_easy,
                                      pb_log = TRUE,
                                      plogger = lgr)


  stillbirthrate_prim_difficult <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                     ps_statement_firstlactation = TRUE,
                                                                     ps_statement_easycalving = FALSE,
                                                                     pb_log = TRUE,
                                                                     plogger = lgr)
  # Check if stillbirthrate_prim_difficult is zero. If it is the case, set a default value
  if(stillbirthrate_prim_difficult == 0){
    stillbirthrate_prim_difficult <- unlist(strsplit(tbl_input_statement_calving[3,4]$input_value, split = " "))[1]
  }
  stillbirthrate_multi_difficult <- qp4ewc::calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                                      ps_statement_firstlactation = FALSE,
                                                                      ps_statement_easycalving = FALSE,
                                                                      pb_log = TRUE,
                                                                      plogger = lgr)
  # Check if stillbirthrate_multi_difficult is zero. If it is the case, set a default value
  if(stillbirthrate_multi_difficult == 0){
    stillbirthrate_multi_difficult <- unlist(strsplit(tbl_input_statement_calving[3,4]$input_value, split = " "))[2]
  }
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
  # Check if calvingdied24h_prop_prim_easy is zero. If it is the case, set a default value
  if(calvingdied24h_prop_prim_easy == 0){
    calvingdied24h_prop_prim_easy <- unlist(strsplit(tbl_input_statement_calving[13,4]$input_value, split = " "))[1]
  }
  calvingdied24h_prop_multi_easy <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                               ps_statement_firstlactation = FALSE,
                                                                               ps_statement_easycalving = TRUE,
                                                                               pb_log = TRUE,
                                                                               plogger = lgr)
  # Check if calvingdied24h_prop_multi_easy is zero. If it is the case, set a default value
  if(calvingdied24h_prop_multi_easy == 0){
    calvingdied24h_prop_multi_easy <- unlist(strsplit(tbl_input_statement_calving[13,4]$input_value, split = " "))[2]
  }
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
  # Check if calvingdied24h_prop_prim_difficult is zero. If it is the case, set a default value
  if(calvingdied24h_prop_prim_difficult == 0){
    calvingdied24h_prop_prim_difficult <- unlist(strsplit(tbl_input_statement_calving[12,4]$input_value, split = " "))[1]
  }
  calvingdied24h_prop_multi_difficult <- qp4ewc::calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                                    ps_statement_firstlactation = FALSE,
                                                                                    ps_statement_easycalving = FALSE,
                                                                                    pb_log = TRUE,
                                                                                    plogger = lgr)
  # Check if calvingdied24h_prop_multi_difficult is zero. If it is the case, set a default value
  if(calvingdied24h_prop_multi_difficult == 0){
    calvingdied24h_prop_multi_difficult <- unlist(strsplit(tbl_input_statement_calving[12,4]$input_value, split = " "))[2]
  }
  value2update_calvingdied24hprop_difficult <- paste0(c(calvingdied24h_prop_prim_difficult, rep(calvingdied24h_prop_multi_difficult,9)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[12,1]),
                                      ps_statement2search = tbl_input_statement_calving[12,2],
                                      ps_value2update = value2update_calvingdied24hprop_difficult,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # Update Losses of calves from 48 hours after calving
  calf_loss <- qp4ewc::calculate_calvesdiedafter24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                               pb_log = TRUE)
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[14,1]),
                                      ps_statement2search = tbl_input_statement_calving[14,2],
                                      ps_value2update = calf_loss,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # Update proportion of cows artificially inseminated in first oestrus
  dystocia_AI <- paste0(c(rep(1,10)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[15,1]),
                                      ps_statement2search = tbl_input_statement_calving[15,2],
                                      ps_value2update = dystocia_AI,
                                      pb_log = TRUE,
                                      plogger = lgr)


  easy_AI <- paste0(c(rep(1,10)),collapse = " ")
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_calving[16,1]),
                                      ps_statement2search = tbl_input_statement_calving[16,2],
                                      ps_value2update = easy_AI,
                                      pb_log = TRUE,
                                      plogger = lgr)


}


#' @title Pre-processing the progeny data flp for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on progeny flp data.
#'
#' @param ps_sirebreed sire breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_progeny_flp_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the slaughter date to filter in the slaughter data
#' @param ps_end_flp_date setting the end of the slaughter date to filter in the slaughter data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ewbc_input_progeny_data_flp
pre_process_ewbc_input_progeny_data_flp <- function(ps_sirebreed,
                                                  ps_prodsystew,
                                                  ps_marketchannel,
                                                  ps_path_directory2create,
                                                  ps_input_file_progeny_flp_statement,
                                                  ps_input_file_flp,
                                                  ps_start_flp_date,
                                                  ps_end_flp_date,
                                                  pb_log = FALSE,
                                                  plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input_progeny_data_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input_progeny_data_flp',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_progeny_flp_statement: ',ps_input_file_progeny_flp_statement, '\n',
                           ' * ps_input_file_flp: ', ps_input_file_flp, '\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n'))
  }


  ### # Read file with statement based on progeny data flp for input-parameter-file of ECOWEIGHT
  tbl_input_statement_flp <- qp4ewc::read_file_input(ps_input_file_progeny_flp_statement,
                                                     pb_log = TRUE,
                                                     plogger = lgr)


  ### # Read file with progeny-flp data
  tbl_flp <- qp4ewc::read_file_input_flp(ps_input_file_flp,
                                         ps_start_flp_date,
                                         ps_end_flp_date,
                                         ps_sirebreed,
                                         pb_log = TRUE,
                                         plogger = lgr)


  ### # Get the constants
  l_constants <- get_constants()


  # ****************************************************************************
  ## ---- Natura-Beef ----
  # ****************************************************************************
  if(ps_marketchannel == "Natura-Beef"){

    # Update statement-progeny-flp-input from the data by calculating mean birth weight
    female_bw <- qp4ewc::calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                                    ps_sex = "F",
                                                    ps_marketing_channel = l_constants$vec_Natura_Beef,
                                                    pb_log = TRUE,
                                                    plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[1,1]),
                                        ps_statement2search = tbl_input_statement_flp[1,2],
                                        ps_value2update = female_bw,
                                        pb_log = TRUE,
                                        plogger = lgr)

    male_bw <- qp4ewc::calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                                  ps_sex = "M",
                                                  ps_marketing_channel = 2,
                                                  pb_log = TRUE,
                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[2,1]),
                                        ps_statement2search = tbl_input_statement_flp[2,2],
                                        ps_value2update = male_bw,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean live weight at slaughter
    livewt_slaughter_f <- qp4ewc::calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_flp,
                                                                      ps_sex = "F",
                                                                      ps_marketing_channel = 2,
                                                                      pb_log = TRUE,
                                                                      plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[3,1]),
                                        ps_statement2search = tbl_input_statement_flp[3,2],
                                        ps_value2update = livewt_slaughter_f,
                                        pb_log = TRUE,
                                        plogger = lgr)

    livewt_slaughter_m <- qp4ewc::calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_flp,
                                                                      ps_sex = "M",
                                                                      ps_marketing_channel = 2,
                                                                      pb_log = TRUE,
                                                                      plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[4,1]),
                                        ps_statement2search = tbl_input_statement_flp[4,2],
                                        ps_value2update = livewt_slaughter_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Calculate weaning weight, weaning age, slaughter age
    weaningwt_f <- qp4ewc::calculate_mean_weaningweight(ps_input_flp_tibble = tbl_flp,
                                                        ps_sex = "F",
                                                        ps_marketing_channel = 2,
                                                        pb_log = TRUE,
                                                        plogger = lgr)
    weaningwt_m <- qp4ewc::calculate_mean_weaningweight(ps_input_flp_tibble = tbl_flp,
                                                        ps_sex = "M",
                                                        ps_marketing_channel = 2,
                                                        pb_log = TRUE,
                                                        plogger = lgr)
    weaningage_f <- qp4ewc::calculate_mean_weaningage(ps_input_flp_tibble = tbl_flp,
                                                      ps_sex = "F",
                                                      ps_marketing_channel = 2,
                                                      pb_log = TRUE,
                                                      plogger = lgr)
    weaningage_m <- qp4ewc::calculate_mean_weaningage(ps_input_flp_tibble = tbl_flp,
                                                      ps_sex = "M",
                                                      ps_marketing_channel = 2,
                                                      pb_log = TRUE,
                                                      plogger = lgr)
    slaughterage_f <- qp4ewc::calculate_mean_slaughterage(ps_input_flp_tibble = tbl_flp,
                                                          ps_sex = "F",
                                                          ps_marketing_channel = 2,
                                                          pb_log = TRUE,
                                                          plogger = lgr)
    slaughterage_m <- qp4ewc::calculate_mean_slaughterage(ps_input_flp_tibble = tbl_flp,
                                                          ps_sex = "M",
                                                          ps_marketing_channel = 2,
                                                          pb_log = TRUE,
                                                          plogger = lgr)


    # Calculate daily gain
    dailygain_f <- qp4ewc::calculate_dailygain(pv_mean_slaughterage = slaughterage_f,
                                               pv_mean_weaningage = weaningage_f,
                                               pv_mean_livewt_atslaughter = livewt_slaughter_f,
                                               pv_mean_weaningwt = weaningwt_f,
                                               pb_log = TRUE,
                                               plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[5,1]),
                                        ps_statement2search = tbl_input_statement_flp[5,2],
                                        ps_value2update = dailygain_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    dailygain_m <- qp4ewc::calculate_dailygain(pv_mean_slaughterage = slaughterage_m,
                                               pv_mean_weaningage = weaningage_m,
                                               pv_mean_livewt_atslaughter = livewt_slaughter_m,
                                               pv_mean_weaningwt = weaningwt_m,
                                               pb_log = TRUE,
                                               plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[6,1]),
                                        ps_statement2search = tbl_input_statement_flp[6,2],
                                        ps_value2update = dailygain_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating extrapolate weaning weight
    # extrapolated to 300 days
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[7,1]),
                                        ps_statement2search = tbl_input_statement_flp[7,2],
                                        ps_value2update = 300,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weight_300d_f <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                                  pv_daily_gain = dailygain_f,
                                                                  pv_mean_weaningwt = weaningwt_f,
                                                                  pv_t_days = 300,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[8,1]),
                                        ps_statement2search = tbl_input_statement_flp[8,2],
                                        ps_value2update = weight_300d_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weight_300d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                                  pv_daily_gain = dailygain_m,
                                                                  pv_mean_weaningwt = weaningwt_m,
                                                                  pv_t_days = 300,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[9,1]),
                                        ps_statement2search = tbl_input_statement_flp[9,2],
                                        ps_value2update = weight_300d_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # extrapolated to 302 days
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[10,1]),
                                        ps_statement2search = tbl_input_statement_flp[10,2],
                                        ps_value2update = 302,
                                        pb_log = TRUE,
                                        plogger = lgr)
    # extrapolated to 304 days
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[11,1]),
                                        ps_statement2search = tbl_input_statement_flp[11,2],
                                        ps_value2update = 304,
                                        pb_log = TRUE,
                                        plogger = lgr)
    # weaning weight at 302 days
    weight_302d_f <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                                  pv_daily_gain = dailygain_f,
                                                                  pv_mean_weaningwt = weaningwt_f,
                                                                  pv_t_days = 302,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[12,1]),
                                        ps_statement2search = tbl_input_statement_flp[12,2],
                                        ps_value2update = weight_302d_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weight_302d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                                  pv_daily_gain = dailygain_m,
                                                                  pv_mean_weaningwt = weaningwt_m,
                                                                  pv_t_days = 302,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[13,1]),
                                        ps_statement2search = tbl_input_statement_flp[13,2],
                                        ps_value2update = weight_302d_m,
                                        pb_log = TRUE,
                                        plogger = lgr)
    # weaning weight at 304 days
    weight_304d_f <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                                  pv_daily_gain = dailygain_f,
                                                                  pv_mean_weaningwt = weaningwt_f,
                                                                  pv_t_days = 304,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[14,1]),
                                        ps_statement2search = tbl_input_statement_flp[14,2],
                                        ps_value2update = weight_304d_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weight_304d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                                  pv_daily_gain = dailygain_m,
                                                                  pv_mean_weaningwt = weaningwt_m,
                                                                  pv_t_days = 304,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[15,1]),
                                        ps_statement2search = tbl_input_statement_flp[15,2],
                                        ps_value2update = weight_304d_m,
                                        pb_log = TRUE,
                                        plogger = lgr)
  }


  # ****************************************************************************
  ## ---- SwissPrimBeef ----
  # ****************************************************************************
  if(ps_marketchannel == "SwissPrimBeef"){

    # Update statement-progeny-flp-input from the data by calculating mean birth weight
    female_bw <- qp4ewc::calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                                    ps_sex = "F",
                                                    ps_marketing_channel = 3,
                                                    pb_log = TRUE,
                                                    plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[1,1]),
                                        ps_statement2search = tbl_input_statement_flp[1,2],
                                        ps_value2update = female_bw,
                                        pb_log = TRUE,
                                        plogger = lgr)

    male_bw <- qp4ewc::calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                                  ps_sex = "M",
                                                  ps_marketing_channel = 3,
                                                  pb_log = TRUE,
                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[2,1]),
                                        ps_statement2search = tbl_input_statement_flp[2,2],
                                        ps_value2update = male_bw,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean weaning weight
    weaningwt_f <- qp4ewc::calculate_mean_weaningweight(ps_input_flp_tibble = tbl_flp,
                                                        ps_sex = "F",
                                                        ps_marketing_channel = 3,
                                                        pb_log = TRUE,
                                                        plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[8,1]),
                                        ps_statement2search = tbl_input_statement_flp[8,2],
                                        ps_value2update = weaningwt_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weaningwt_m <- qp4ewc::calculate_mean_weaningweight(ps_input_flp_tibble = tbl_flp,
                                                        ps_sex = "M",
                                                        ps_marketing_channel = 3,
                                                        pb_log = TRUE,
                                                        plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[9,1]),
                                        ps_statement2search = tbl_input_statement_flp[9,2],
                                        ps_value2update = weaningwt_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean weaning age
    weaningage_f <- qp4ewc::calculate_mean_weaningage(ps_input_flp_tibble = tbl_flp,
                                                      ps_sex = "F",
                                                      ps_marketing_channel = 3,
                                                      pb_log = TRUE,
                                                      plogger = lgr)
    weaningage_m <- qp4ewc::calculate_mean_weaningage(ps_input_flp_tibble = tbl_flp,
                                                      ps_sex = "M",
                                                      ps_marketing_channel = 3,
                                                      pb_log = TRUE,
                                                      plogger = lgr)
    weaningage_average <- round(as.numeric(weaningage_f+weaningage_m)/2,4)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[7,1]),
                                        ps_statement2search = tbl_input_statement_flp[7,2],
                                        ps_value2update = weaningage_average,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean live weight at slaughter
    livewt_slaughter_f <- qp4ewc::calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_flp,
                                                                      ps_sex = "F",
                                                                      ps_marketing_channel = 3,
                                                                      pb_log = TRUE,
                                                                      plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[3,1]),
                                        ps_statement2search = tbl_input_statement_flp[3,2],
                                        ps_value2update = livewt_slaughter_f,
                                        pb_log = TRUE,
                                        plogger = lgr)

    livewt_slaughter_m <- qp4ewc::calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_flp,
                                                                      ps_sex = "M",
                                                                      ps_marketing_channel = 3,
                                                                      pb_log = TRUE,
                                                                      plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[4,1]),
                                        ps_statement2search = tbl_input_statement_flp[4,2],
                                        ps_value2update = livewt_slaughter_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Calculate slaughter age
    slaughterage_f <- qp4ewc::calculate_mean_slaughterage(ps_input_flp_tibble = tbl_flp,
                                                          ps_sex = "F",
                                                          ps_marketing_channel = 3,
                                                          pb_log = TRUE,
                                                          plogger = lgr)
    slaughterage_m <- qp4ewc::calculate_mean_slaughterage(ps_input_flp_tibble = tbl_flp,
                                                          ps_sex = "M",
                                                          ps_marketing_channel = 3,
                                                          pb_log = TRUE,
                                                          plogger = lgr)


    # Calculate and update daily gain
    dailygain_f <- qp4ewc::calculate_dailygain(pv_mean_slaughterage = slaughterage_f,
                                               pv_mean_weaningage = weaningage_f,
                                               pv_mean_livewt_atslaughter = livewt_slaughter_f,
                                               pv_mean_weaningwt = weaningwt_f,
                                               pb_log = TRUE,
                                               plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[5,1]),
                                        ps_statement2search = tbl_input_statement_flp[5,2],
                                        ps_value2update = dailygain_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    dailygain_m <- qp4ewc::calculate_dailygain(pv_mean_slaughterage = slaughterage_m,
                                               pv_mean_weaningage = weaningage_m,
                                               pv_mean_livewt_atslaughter = livewt_slaughter_m,
                                               pv_mean_weaningwt = weaningwt_m,
                                               pb_log = TRUE,
                                               plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[6,1]),
                                        ps_statement2search = tbl_input_statement_flp[6,2],
                                        ps_value2update = dailygain_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating extrapolate weaning weight
    # extrapolated to 300 days
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[10,1]),
                                        ps_statement2search = tbl_input_statement_flp[10,2],
                                        ps_value2update = 300,
                                        pb_log = TRUE,
                                        plogger = lgr)
    # extrapolated to 400 days
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[11,1]),
                                        ps_statement2search = tbl_input_statement_flp[11,2],
                                        ps_value2update = 400,
                                        pb_log = TRUE,
                                        plogger = lgr)
    # weaning weight at 300 days
    weight_300d_f <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                                  pv_daily_gain = dailygain_f,
                                                                  pv_mean_weaningwt = weaningwt_f,
                                                                  pv_t_days = 300,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[12,1]),
                                        ps_statement2search = tbl_input_statement_flp[12,2],
                                        ps_value2update = weight_300d_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weight_300d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                                  pv_daily_gain = dailygain_m,
                                                                  pv_mean_weaningwt = weaningwt_m,
                                                                  pv_t_days = 300,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[13,1]),
                                        ps_statement2search = tbl_input_statement_flp[13,2],
                                        ps_value2update = weight_300d_m,
                                        pb_log = TRUE,
                                        plogger = lgr)


    # weaning weight at 400 days
    weight_400d_f <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                                  pv_daily_gain = dailygain_f,
                                                                  pv_mean_weaningwt = weaningwt_f,
                                                                  pv_t_days = 400,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[14,1]),
                                        ps_statement2search = tbl_input_statement_flp[14,2],
                                        ps_value2update = weight_400d_f,
                                        pb_log = TRUE,
                                        plogger = lgr)
    weight_400d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                                  pv_daily_gain = dailygain_m,
                                                                  pv_mean_weaningwt = weaningwt_m,
                                                                  pv_t_days = 400,
                                                                  pb_log = TRUE,
                                                                  plogger = lgr)
    qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[15,1]),
                                        ps_statement2search = tbl_input_statement_flp[15,2],
                                        ps_value2update = weight_400d_m,
                                        pb_log = TRUE,
                                        plogger = lgr)
  }



  # Update statement-progeny-flp-input from the data by calculating cow weight after second calving
  second_calving_wt <- qp4ewc::calculate_cow_liveweight(ps_input_flp_tibble = tbl_flp,
                                                        ps_second_calvingweight = TRUE,
                                                        pb_log = TRUE,
                                                        plogger = lgr)
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[16,1]),
                                      ps_statement2search = tbl_input_statement_flp[16,2],
                                      ps_value2update = second_calving_wt,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # Update statement-progeny-flp-input from the data by calculating mature cow weight
  mature_weight_cow <- qp4ewc::calculate_cow_liveweight(ps_input_flp_tibble = tbl_flp,
                                                        ps_second_calvingweight = FALSE,
                                                        pb_log = TRUE,
                                                        plogger = lgr)
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[17,1]),
                                      ps_statement2search = tbl_input_statement_flp[17,2],
                                      ps_value2update = mature_weight_cow,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # Update statement-progeny-flp-input from the data by calculating mature bull weight
  bull_mature_weight <- qp4ewc::calculate_bull_liveweight(ps_input_flp_tibble = tbl_flp,
                                                          pb_log = TRUE,
                                                          plogger = lgr)
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp[18,1]),
                                      ps_statement2search = tbl_input_statement_flp[18,2],
                                      ps_value2update = bull_mature_weight,
                                      pb_log = TRUE,
                                      plogger = lgr)



}


#' @title Pre-processing the carcass conformation, fat, prices based on flp-data for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on progeny flp data and literature about the price-system.
#'
#' @param ps_sirebreed sire breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_flp_carcass_matrix_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the slaughter date to filter in the slaughter data
#' @param ps_end_flp_date setting the end of the slaughter date to filter in the slaughter data
#' @param ps_input_file_price_cow path to file with price for cows
#' @param ps_input_file_price_bull path to file with price for bulls
#' @param ps_input_file_price_heifer path tho file with price for heifers
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ew_input_carcass_data_flp
pre_process_ew_input_carcass_data_flp <- function(ps_sirebreed,
                                                  ps_prodsystew,
                                                  ps_marketchannel,
                                                  ps_path_directory2create,
                                                  ps_input_file_flp_carcass_matrix_statement,
                                                  ps_input_file_flp,
                                                  ps_start_flp_date,
                                                  ps_end_flp_date,
                                                  ps_input_file_price_cow,
                                                  ps_input_file_price_bull,
                                                  ps_input_file_price_heifer,
                                                  pb_log = FALSE,
                                                  plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input_carcass_data_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input_carcass_data_flp',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_flp_carcass_matrix_statement: ',ps_input_file_flp_carcass_matrix_statement, '\n',
                           ' * ps_input_file_flp: ', ps_input_file_flp, '\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n',
                           ' * ps_input_file_price_cow: ',ps_input_file_price_cow,'\n',
                           ' * ps_input_file_price_bull: ',ps_input_file_price_bull,'\n',
                           ' * ps_input_file_price_heifer: ',ps_input_file_price_heifer))
  }


  ### # Read file with statement based on carcass data flp for input-parameter-file of ECOWEIGHT
  tbl_input_statement_flp_carcass <- qp4ewc::read_file_input(ps_input_file_flp_carcass_matrix_statement,
                                                             pb_log = TRUE,
                                                             plogger = lgr)


  ### # Read file with progeny-flp data
  tbl_flp <- qp4ewc::read_file_input_flp(ps_input_file_flp,
                                         ps_start_flp_date,
                                         ps_end_flp_date,
                                         ps_sirebreed,
                                         pb_log = TRUE,
                                         plogger = lgr)


  ### # Number of classes for fleshiness
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[11,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[11,2],
                                      ps_value2update = tbl_input_statement_flp_carcass[11,4],
                                      pb_log = TRUE,
                                      plogger = lgr)


  ### # Number of classes for fat covering
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[12,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[12,2],
                                      ps_value2update = tbl_input_statement_flp_carcass[12,4],
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Cows frequencies ----
  # ****************************************************************************
  freq_mat_cow <- qp4ewc::build_freq_conf_fat(ps_input_flp_tibble = tbl_flp,
                                              ps_sex = "F",
                                              ps_marketing_channel = NULL,
                                              ps_flag_cow = TRUE,
                                              pb_log = TRUE,
                                              plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[1,],4)),collapse = " "),
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation H
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[2,],4)),collapse = " "),
                                      ps_line4statement2update = 2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T+
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[3,],4)),collapse = " "),
                                      ps_line4statement2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[4,],4)),collapse = " "),
                                      ps_line4statement2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T-
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[5,],4)),collapse = " "),
                                      ps_line4statement2update = 5,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation A
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[6,],4)),collapse = " "),
                                      ps_line4statement2update = 6,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation X
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[7,],4)),collapse = " "),
                                      ps_line4statement2update = 7,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[8,],4)),collapse = " "),
                                      ps_line4statement2update = 8,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XXX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[1,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[1,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_cow[9,],4)),collapse = " "),
                                      ps_line4statement2update = 9,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Heifers frequencies ----
  # ****************************************************************************
  freq_mat_heifer <- qp4ewc::build_freq_conf_fat(ps_input_flp_tibble = tbl_flp,
                                                 ps_sex = "F",
                                                 ps_marketchannel,
                                                 ps_flag_cow = FALSE,
                                                 pb_log = TRUE,
                                                 plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[1,],4)),collapse = " "),
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation H
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[2,],4)),collapse = " "),
                                      ps_line4statement2update = 2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T+
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[3,],4)),collapse = " "),
                                      ps_line4statement2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[4,],4)),collapse = " "),
                                      ps_line4statement2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T-
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[5,],4)),collapse = " "),
                                      ps_line4statement2update = 5,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation A
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[6,],4)),collapse = " "),
                                      ps_line4statement2update = 6,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation X
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[7,],4)),collapse = " "),
                                      ps_line4statement2update = 7,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[8,],4)),collapse = " "),
                                      ps_line4statement2update = 8,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XXX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[3,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[3,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_heifer[9,],4)),collapse = " "),
                                      ps_line4statement2update = 9,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Bulls frequencies ----
  # ****************************************************************************
  freq_mat_bull <- qp4ewc::build_freq_conf_fat(ps_input_flp_tibble = tbl_flp,
                                               ps_sex = "M",
                                               ps_marketchannel,
                                               ps_flag_cow = FALSE,
                                               pb_log = TRUE,
                                               plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[1,],4)),collapse = " "),
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation H
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[2,],4)),collapse = " "),
                                      ps_line4statement2update = 2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T+
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[3,],4)),collapse = " "),
                                      ps_line4statement2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[4,],4)),collapse = " "),
                                      ps_line4statement2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T-
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[5,],4)),collapse = " "),
                                      ps_line4statement2update = 5,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation A
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[6,],4)),collapse = " "),
                                      ps_line4statement2update = 6,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation X
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[7,],4)),collapse = " "),
                                      ps_line4statement2update = 7,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[8,],4)),collapse = " "),
                                      ps_line4statement2update = 8,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XXX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[2,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[2,2],
                                      ps_value2update = paste0(as.character(round(freq_mat_bull[9,],4)),collapse = " "),
                                      ps_line4statement2update = 9,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Prices ----
  # ****************************************************************************
  # Update statement_flp_carcass-input from the data
  # base price for cow
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[4,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[4,2],
                                      ps_value2update = tbl_input_statement_flp_carcass[4,4],
                                      pb_log = TRUE,
                                      plogger = lgr)
  # average price bull
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[5,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[5,2],
                                      ps_value2update = tbl_input_statement_flp_carcass[5,4],
                                      pb_log = TRUE,
                                      plogger = lgr)
  # basis price bull
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[6,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[6,2],
                                      ps_value2update = tbl_input_statement_flp_carcass[6,4],
                                      pb_log = TRUE,
                                      plogger = lgr)
  # basis price heifer
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[7,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[7,2],
                                      ps_value2update = tbl_input_statement_flp_carcass[7,4],
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for cow----
  # ****************************************************************************
  mat_coeffprice_cow <- qp4ewc::read_price_conf_fat(ps_input_file_price = ps_input_file_price_cow,
                                                    pb_log = TRUE,
                                                    plogger = lgr)
  # Update price coefficient for cow
  ### # For carcass conformation C
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[1,],4)),collapse = " "),
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation H
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[2,],4)),collapse = " "),
                                      ps_line4statement2update = 2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T+
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[3,],4)),collapse = " "),
                                      ps_line4statement2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[4,],4)),collapse = " "),
                                      ps_line4statement2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T-
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[5,],4)),collapse = " "),
                                      ps_line4statement2update = 5,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation A
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[6,],4)),collapse = " "),
                                      ps_line4statement2update = 6,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation X
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[7,],4)),collapse = " "),
                                      ps_line4statement2update = 7,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[8,],4)),collapse = " "),
                                      ps_line4statement2update = 8,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XXX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[8,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[8,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_cow[9,],4)),collapse = " "),
                                      ps_line4statement2update = 9,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for bull----
  # ****************************************************************************
  mat_coeffprice_bull <- qp4ewc::read_price_conf_fat(ps_input_file_price = ps_input_file_price_bull,
                                                    pb_log = TRUE,
                                                    plogger = lgr)
  # Update price coefficient for bull
  ### # For carcass conformation C
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[1,],4)),collapse = " "),
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation H
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[2,],4)),collapse = " "),
                                      ps_line4statement2update = 2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T+
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[3,],4)),collapse = " "),
                                      ps_line4statement2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[4,],4)),collapse = " "),
                                      ps_line4statement2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T-
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[5,],4)),collapse = " "),
                                      ps_line4statement2update = 5,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation A
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[6,],4)),collapse = " "),
                                      ps_line4statement2update = 6,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation X
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[7,],4)),collapse = " "),
                                      ps_line4statement2update = 7,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[8,],4)),collapse = " "),
                                      ps_line4statement2update = 8,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XXX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[9,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[9,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_bull[9,],4)),collapse = " "),
                                      ps_line4statement2update = 9,
                                      pb_log = TRUE,
                                      plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for heifer----
  # ****************************************************************************
  mat_coeffprice_heifer <- qp4ewc::read_price_conf_fat(ps_input_file_price = ps_input_file_price_heifer,
                                                     pb_log = TRUE,
                                                     plogger = lgr)
  # Update price coefficient for bull
  ### # For carcass conformation C
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[1,],4)),collapse = " "),
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation H
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[2,],4)),collapse = " "),
                                      ps_line4statement2update = 2,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T+
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[3,],4)),collapse = " "),
                                      ps_line4statement2update = 3,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[4,],4)),collapse = " "),
                                      ps_line4statement2update = 4,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation T-
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[5,],4)),collapse = " "),
                                      ps_line4statement2update = 5,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation A
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[6,],4)),collapse = " "),
                                      ps_line4statement2update = 6,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation X
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[7,],4)),collapse = " "),
                                      ps_line4statement2update = 7,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[8,],4)),collapse = " "),
                                      ps_line4statement2update = 8,
                                      pb_log = TRUE,
                                      plogger = lgr)
  ### # For carcass conformation XXX
  qp4ewc::update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[10,1]),
                                      ps_statement2search = tbl_input_statement_flp_carcass[10,2],
                                      ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[9,],4)),collapse = " "),
                                      ps_line4statement2update = 9,
                                      pb_log = TRUE,
                                      plogger = lgr)


}
