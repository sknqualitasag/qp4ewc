### #
### #
### #
### #   Purpose:   Function related to the changement of input parameter
### #   started:   2022-09-20 (skn)
### #
### # ##################################################################### ###


#ps_path_input_paramfile <- file.path(here::here(),"inst","extdata","ewdc","test","Ecoweight_output")
#ps_name_input_paramfile_2change <- system.file("extdata","ewdc",paste0("input_literature_",ps_dambreed,"_",ps_marketchannel,".txt"), package = "qp4ewc")
#ps_name_input_paramfile_changed <- file.path(here::here(),"inst","extdata","ewdc","test","Preprocess_output",paste0("input_literature_",ps_dambreed,"_",ps_marketchannel,"_dayP1.txt"))
#
#
#ps_sirebreed <- "LM"
#ps_dambreed <- "HO"
#ps_prodsystew <- "4"
#ps_marketchannel <- "ConventionalBeef"
#ps_path_input_paramfile <- file.path(here::here(),"inst","extdata","ewdc")
#ps_name_input_paramfile_2change <- paste0("input_literature_",ps_dambreed,"_",ps_marketchannel,".txt")
#ps_name_input_paramfile_changed <- paste0("input_literature_",ps_dambreed,"_",ps_marketchannel,"_dayP1.txt")
#ps_statement2search <- "Gestation length"
#ps_value2change <- 1
#pb_log <- TRUE
#plogger <- NULL


#' @title Change input-parameter-file for ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will change the input parameter. 
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_name_input_paramfile_2change path to input-parameter-file to change for ECOWEIGHT
#' @param ps_name_input_paramfile_changed path to input-parameter-file changed for ECOWEIGHT
#' @param ps_statement2search statement to search
#' @param ps_value2change how to change the value
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#' 
#' @importFrom dplyr %>%
#' @import dplyr
#' @import readr
#'
#' @export change_input_paramfile
change_input_paramfile <- function(ps_sirebreed,
                                   ps_dambreed,
                                   ps_prodsystew,
                                   ps_marketchannel,
                                   ps_path_input_paramfile,
                                   ps_name_input_paramfile_2change,
                                   ps_name_input_paramfile_changed,
                                   ps_statement2search,
                                   ps_value2change,
                                   pb_log,
                                   plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'change_input_paramfile.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'change_input_paramfile',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_input_paramfile: ', ps_path_input_paramfile, '\n',
                           ' * ps_name_input_paramfile_2change: ', ps_name_input_paramfile_2change, '\n',
                           ' * ps_name_input_paramfile_changed: ', ps_name_input_paramfile_changed, '\n',
                           ' * ps_statement2search: ', ps_statement2search, '\n',
                           ' * ps_value2change: ', ps_value2change, '\n'))
  }
  
  
  ### # Check if file exist otherwise stop running the function
  if(!file.exists(file.path(ps_path_input_paramfile,ps_name_input_paramfile_2change))){
    stop("read_file_input: file ",file.path(ps_path_input_paramfile,ps_name_input_paramfile_2change)," does not exist, please check the path !")
  }else{
    if(pb_log){
      qp4ewc_log_info(lgr, 'read_file_input',paste0('File exists:\n * file.path(ps_path_input_paramfile,ps_name_input_paramfile_2change)',file.path(ps_path_input_paramfile,ps_name_input_paramfile_2change)))
    }
  }
  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_input_paramfile_2change <- read_file_input(ps_input_file = file.path(ps_path_input_paramfile,ps_name_input_paramfile_2change),
                                                 pb_log = pb_log,
                                                 plogger = lgr)
  
  
  ### # Search the statement
  tbl_statement2search <- tbl_input_paramfile_2change %>% dplyr::filter(input == ps_statement2search)
  ### # Change the input-value
  n_value <- as.character(as.numeric(tbl_statement2search$input_value)+ps_value2change)
  
  
  ### # Build up the changed tibble
  tbl_input_paramfile_changed <- tbl_input_paramfile_2change %>% dplyr::mutate(input_value=replace(input_value,input == ps_statement2search,n_value))
  
  
  
  ### # write the new parameter-file with the change
  ### # Check the path extension .txt
  if(grepl("\\.txt$", file.path(ps_path_input_paramfile,ps_name_input_paramfile_2change))){
    ### # write the tibble in a file
    readr::write_delim(x = tbl_input_paramfile_changed, file = file.path(ps_path_input_paramfile,ps_name_input_paramfile_changed), delim = ";")
  }
  

}