### #
### #
### #
### #   Purpose:   Functions Related to Logging
### #   started:   2022-03-04 (skn)
### #
### # ############################################## ###


#' @title Create log4r Logger for package
#'
#' @param ps_logfile name of the logfile
#' @param ps_level logger level
#'
#' @return qp4ewc_logger
#' @export get_qp4ewc_logger
get_qp4ewc_logger <- function(ps_logfile = 'qp4ewc.log', ps_level = 'FATAL'){
  qp4ewc_logger <- log4r::create.logger(logfile = ps_logfile, level = ps_level)
  return(qp4ewc_logger)
}


#' @title Wrapper for log4r info
#'
#' @param plogger log4r logger object
#' @param ps_msg logging message
#'
#' @export qp4ewc_log_info
#'
qp4ewc_log_info <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}
