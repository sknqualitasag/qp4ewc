### #
### #
### #
### #   Purpose:   Function related to the input for input-parameter of ECOWEIGHT
### #   started:   2022-03-04 (skn)
### #
### # ##################################################################### ###


#' @title Read file with input from literature research for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will read a file with value coming from literature research.
#'
#' @param ps_input_file_literature path to file with input coming from literature for the input-parameter-file for ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @return tibble with the content of the literatur file
#'
#' @export read_file_input_literature
read_file_input_literature <-  function(ps_input_file_literature,
                                        pb_log = FALSE,
                                        plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'read_file_input_literature.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'read_file_input_literature',
                    paste0('Starting function with parameters:\n * ps_input_file_literature: ', ps_input_file_literature))
  }


  ### # Check if file exist otherwise stop running the function
  if(!file.exists(ps_input_file_literature)){
    stop("read_file_input_literature: file ",ps_input_file_literature," does not exist, please check the path !")
  }else{
    qp4ewc_log_info(lgr, 'read_file_input_literature',paste0('File exists:\n * ps_input_file_literature',ps_input_file_literature))
  }


  ### # Read the input file with literature values
  tbl_input <- readr::read_delim(file = ps_input_file_literature, delim = ";")
  qp4ewc_log_info(lgr, 'read_file_input_literature',paste0('Read file: \n * ps_input_file_literature: ',ps_input_file_literature,"\n",
                                                           ' * in a tibble','\n'))


  ### # Return tibble
  return(tbl_input)

}



#' @title Read file with input about calving for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will read a file with value coming from calving.
#' In this calving file some information about abortion rate, stillbirth rate, calving score,
#' amount of calves died within 48 hours after birth.
#'
#' @param ps_input_file_calving path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @return tibble with the content of the calving file
#'
#' @export read_file_input_calving
read_file_input_calving <-  function(ps_input_file_calving,
                                     pb_log = FALSE,
                                     plogger = NULL){


  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'read_file_input_calving.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'read_file_input_calving',
                    paste0('Starting function with parameters:\n * ps_input_file_calving: ', ps_input_file_calving))
  }


  ### # Check if file exist otherwise stop running the function
  if(!file.exists(ps_input_file_calving)){
    stop("read_file_input_calving: file ",ps_input_file_calving," does not exist, please check the path !")
  }else{
    qp4ewc_log_info(lgr, 'read_file_input_calving',paste0('File exists:\n * ps_input_file_calving',ps_input_file_calving))
  }


  ### # Read the input calving file
  tbl_input <- readr::read_delim(file = ps_input_file_calving, delim = ";")
  qp4ewc_log_info(lgr, 'read_file_input_calving',paste0('Read file: \n * ps_input_file_calving: ',ps_input_file_calving,"\n",
                                                           ' * in a tibble','\n'))


  ### # Return tibble
  return(tbl_input)


}
