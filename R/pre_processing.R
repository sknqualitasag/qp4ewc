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
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ew_input
pre_process_ew_input <- function(ps_sirebreed,
                                 ps_prodsystew,
                                 ps_marketchannel,
                                 ps_path_directory2create,
                                 ps_input_file_literature,
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
                           ' * ps_input_file_literature: ', ps_input_file_literature, '\n'))
  }


  ### # Create directory per scenario with input-parameter-file for ECOWEIGHT
  qp4ewc::create_directory_scenario(ps_sirebreed,
                                    ps_prodsystew,
                                    ps_marketchannel,
                                    ps_path_directory2create,
                                    pb_log = TRUE,
                                    plogger = lgr)


  ### # Read file with input from literature research for input-parameter-file of ECOWEIGHT
  tbl_input_literature <- qp4ewc::read_file_input_literature(ps_input_file_literature,
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

}
