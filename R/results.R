### #
### #
### #
### #   Purpose:   Functions related to the results of ECOWEIGHT
### #   started:   2022-05-20 (skn)
### #
### # ##################################################################### ###


#' @title Extract the part of interest of the results coming from ECOWEIGHT output
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce ouptut file with the results. This function will extract the results needed.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_start_statement2extract string as statement to set the beginning of the extraction
#' @param ps_end_statement2extract string as statement to set the ending of the extraction
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @return vec_ecow_result_2extract
#'
#' @export extract_result
extract_result <- function(ps_path_2outputfile,
                           ps_start_statement2extract,
                           ps_end_statement2extract,
                           pb_log = FALSE,
                           plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_result.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_result',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_start_statement2extract: ', ps_start_statement2extract, '\n',
                           ' * ps_end_statement2extract: ', ps_end_statement2extract))
  }


  ### # Check if output file exist otherwise stop running the function
  if(!file.exists(ps_path_2outputfile)){
    stop("extract_result: file ",ps_path_2outputfile," does not exist, please check the path !")
  }else{
    if(pb_log){
      qp4ewc_log_info(lgr, 'extract_result',paste0('File exists:\n * ps_path_2outputfile',ps_path_2outputfile))
    }
  }


  ### # Build up a vector with the output-parameter-file where each line of the input has a belonging number
  vec_ecow_result <- readLines(con = file(description = ps_path_2outputfile))


  ### # Surch the statement in the output-parameter-file
  n_idx_start <- grep(pattern = ps_start_statement2extract, vec_ecow_result, fixed = TRUE)
  n_idx_end <- grep(pattern = ps_end_statement2extract, vec_ecow_result, fixed = TRUE)


  ### # Exclude everything outside of result area
  vec_ecow_result_2extract <- vec_ecow_result[n_idx_start:n_idx_end]


  ### # Return vector
  return(vec_ecow_result_2extract)


}
