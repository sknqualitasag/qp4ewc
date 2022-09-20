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
#' produce output file with the results. This function will extract the results needed.
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



#' @title Get the value of interest of the results coming from ECOWEIGHT output
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file with the results. This function will extract the value needed.
#'
#' @param pvec_ecow_result_2extract vector result of output file of ECOWEIGHT
#' @param ps_statement2search tibble with the parttern to search
#' @param ps_line2get line to get
#' @param ps_splitby string to say how it is splitted
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @return vec_cur_result
#'
#' @export get_result_value
get_result_value <- function(pvec_ecow_result_2extract,
                             ps_statement2search,
                             ps_line2get,
                             ps_splitby,
                             pb_log = FALSE,
                             plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'get_result_value.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'get_result_value',
                    paste0('Starting function with parameters:\n * pvec_ecow_result_2extract \n',
                           ' * ps_statement2search \n',
                           ' * ps_line2get: ',ps_line2get,'\n',
                           ' * ps_splitby: ',ps_splitby,'\n'))
  }


  ### # Find in the output vector based on the tibble with the information to search and the line after the statement to get
  n_cur_trait_idx <- grep(pattern = ps_statement2search, pvec_ecow_result_2extract, fixed = TRUE) + ps_line2get
  s_cur_result <- pvec_ecow_result_2extract[n_cur_trait_idx]
  ### # Get the value
  vec_cur_result <- unlist(strsplit(s_cur_result, split = ps_splitby, fixed = FALSE))

  return(vec_cur_result)


}

