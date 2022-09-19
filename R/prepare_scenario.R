### #
### #
### #
### #   Purpose:   Function related to the preparation of environment of each scenario to run ECOWEIGHT
### #   started:   2022-03-04 (skn)
### #
### # ##################################################################### ###


#' @title Create directory per scenario with input-parameter-file for ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will create a directory with a specific name
#' to recognize each scenario and file this directory with required input-parameter-file
#' to run ECOWEIGHT.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export create_directory_scenario
create_directory_scenario <- function(ps_sirebreed,
                                      ps_dambreed,
                                      ps_prodsystew,
                                      ps_marketchannel,
                                      ps_path_directory2create,
                                      pb_log = FALSE,
                                      plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'create_directory_scenario.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'create_directory_scenario',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create))
  }


  ### # Build a name for each scenario
  s_scenario <- file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed, "_",ps_prodsystew,"_",ps_marketchannel))
  if(pb_log){
    qp4ewc_log_info(lgr, 'create_directory_scenario',
                    paste0('Define the name of the scenario:\n * s_scenario: ', s_scenario))
  }


  ### # Create a directory per scenario
  if(file.exists(s_scenario)){
    stop("The directory ",s_scenario," exists already!")
  }else{
    dir.create(s_scenario, recursive = TRUE)
    if(pb_log){
      qp4ewc_log_info(lgr, 'create_directory_scenario',
                      paste0('Create a directory for scenario:\n * s_scenario: ', s_scenario))
    }
  }


  ### # Get the constants
  l_constants <- get_constants()


  ### # Copy the default parameter-input-files from the template directory to the scenario directory
  ### # ECOWEIGHT for beef cattle: EWBC
  if(ps_prodsystew != l_constants$beefOndairy){
    list_of_files <- list.files(system.file("templates","ewbc", package = "qp4ewc"), full.names = TRUE)
    file.copy(list_of_files,s_scenario)
    if(pb_log){
      qp4ewc_log_info(lgr, 'create_directory_scenario',
                      paste0('Copy the default input-parameter-files in the scenario directory based on the templates for ewbc','\n'))
    }
  }

  if(ps_prodsystew == l_constants$beefOndairy){
    list_of_files <- list.files(system.file("templates", "ewdc", package = "qp4ewc"), full.names = TRUE)
    file.copy(list_of_files,s_scenario)
    if(pb_log){
      qp4ewc_log_info(lgr, 'create_directory_scenario',
                      paste0('Copy the default input-parameter-files in the scenario directory based on the templates for ewdc','\n'))
    }

  }
}


