### #
### #
### #
### #   Purpose:   Function related to the updating of input-parameter of ECOWEIGHT
### #   started:   2022-03-04 (skn)
### #
### # ##################################################################### ###


#' @title Update input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will search a specific statement in a template and
#' will update the value for the input parameter files.
#'
#' @param ps_path2template_input_parameter_file path to the template of the input-parameter-file for ECOWEIGHT
#' @param ps_statement2search statement to search in the template
#' @param ps_value2update value to update in the input-parameter-file
#' @param ps_line4statement2update numeric value of the number of the line before the statement to update
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export update_input_parameter_file
update_input_parameter_file <- function(ps_path2template_input_parameter_file,
                                        ps_statement2search,
                                        ps_value2update,
                                        ps_line4statement2update = 1,
                                        pb_log = FALSE,
                                        plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'update_input_parameter_file.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'update_input_parameter_file',
                    paste0('Starting function with parameters:\n * ps_path2template_input_parameter_file: ', ps_path2template_input_parameter_file, '\n',
                           ' * ps_statement2search: ', ps_statement2search, '\n',
                           ' * ps_value2update: ', ps_value2update))
  }


  ### # Check if file exist otherwise stop running the function
  if(!file.exists(ps_path2template_input_parameter_file)){
    stop("update_input_parameter_file: file ",ps_path2template_input_parameter_file," does not exist, please check the path !")
  }else{
    if(pb_log){
      qp4ewc_log_info(lgr, 'update_input_parameter_file',paste0('File exists:\n * ps_path2template_input_parameter_file',ps_path2template_input_parameter_file))
    }
  }


  ### # Build up a vector with the input-parameter-file where each line of the input has a belonging number
  vec_ecow_input <- readLines(con = file(description = ps_path2template_input_parameter_file))


  ### # Surch the statement in the input-parameter-file
  vec_linenb_statement2search <-grep(pattern = ps_statement2search , vec_ecow_input, fixed = TRUE)
  if(pb_log){
    qp4ewc_log_info(lgr, 'update_input_parameter_file',paste0('Surch:\n * ps_statement2search: ',ps_statement2search, '\n',
                                                              ' * Find statement on vector-line: ',vec_linenb_statement2search))
  }


  ### # Update the value in the input-parameter-file
  ps_value2update_idx <- grep(pattern = ps_statement2search , vec_ecow_input, fixed = TRUE) - ps_line4statement2update
  vec_ecow_input[ps_value2update_idx] <- ps_value2update
  cat(paste0(vec_ecow_input, collapse = "\n"), "\n", file = ps_path2template_input_parameter_file, append = FALSE)
  if(pb_log){
    qp4ewc_log_info(lgr, 'update_input_parameter_file',paste0('Update the value:\n * ps_value2update: ',ps_value2update, '\n',
                                                              ' * of the statement ps_statement2search: ',ps_statement2search, '\n',
                                                              ' * on vector-line: ',ps_value2update_idx,'\n'))
  }

}
