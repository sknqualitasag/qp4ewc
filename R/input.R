### #
### #
### #
### #   Purpose:   Function related to the input for input-parameter of ECOWEIGHT
### #   started:   2022-03-04 (skn)
### #
### # ##################################################################### ###


#' @title Read file with input for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will read a file with value .
#'
#' @param ps_input_file path to file with input for the input-parameter-file for ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @return tibble with the content of the file
#'
#' @export read_file_input
read_file_input <-  function(ps_input_file,
                             pb_log = FALSE,
                             plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'read_file_input.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'read_file_input',
                    paste0('Starting function with parameters:\n * ps_input_file: ', ps_input_file))
  }


  ### # Check if file exist otherwise stop running the function
  if(!file.exists(ps_input_file)){
    stop("read_file_input: file ",ps_input_file," does not exist, please check the path !")
  }else{
    qp4ewc_log_info(lgr, 'read_file_input',paste0('File exists:\n * ps_input_file',ps_input_file))
  }


  ### # Read the input file with literature values
  tbl_input <- readr::read_delim(file = ps_input_file, delim = ";")
  qp4ewc_log_info(lgr, 'read_file_input',paste0('Read file: \n * ps_input_file: ',ps_input_file,"\n",
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
#' @param ps_start_calving_date setting the start of the calving date to filter in the calving data
#' @param ps_end_calving_date setting the end of the calving date to filter in the calving data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return tibble with the content of the calving file
#'
#' @export read_file_input_calving
read_file_input_calving <-  function(ps_input_file_calving,
                                     ps_start_calving_date,
                                     ps_end_calving_date,
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
                    paste0('Starting function with parameters:\n * ps_input_file_calving: ', ps_input_file_calving,'\n',
                           ' * ps_start_calving_date: ',ps_start_calving_date,'\n',
                           ' * ps_end_calving_date: ',ps_end_calving_date))
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
                                                           ' * in a tibble'))

  ### # Check if some columns-header are available in the input calving file
  vec_calvingHeader_name <- names(tbl_input)
  vec_requested_calvingHeader_name <- c("Abkalbedatum","Geburtsverlauf","Laktationsnummer_Mutter","Abort","Code_TotOLebend",
                                        "Nachkomme_RasseCode","Mutter_RasseCode","Vater_RasseCode","Geschlecht")
  if(all(vec_requested_calvingHeader_name %in% vec_calvingHeader_name)){
    qp4ewc_log_info(lgr, 'read_file_input_calving',paste0('All requested column-names in calving input file exist'))
  }else{
    stop("read_file_input_calving: Not all requested column-names in calving input file exist, please check the file !")
  }


  ### # Selection criteria on the input calving file
  ### # Specific date interval to consider in the data
  tbl_input <- tbl_input %>% dplyr::filter(Abkalbedatum >= ps_start_calving_date) %>% dplyr::filter(Abkalbedatum <= ps_end_calving_date)
  qp4ewc_log_info(lgr, 'read_file_input_calving',paste0('Considered data from input calving file from: ',ps_start_calving_date, ' to ', ps_end_calving_date))


  ### # Return tibble
  return(tbl_input)


}


#' @title Compute Age in Days
#'
#' @description
#' By default the age in days is computed. If age on different date should be
#' computed use pdate_today with a different values. In case you want to get
#' a real number as the age, then use pb_floor = FALSE.
#'
#' The function is based on https://stackoverflow.com/questions/14454476/get-the-difference-between-dates-in-terms-of-weeks-months-quarters-and-years
#'
#' @param pdate_birth date of birth
#' @param pdate_today todays date
#' @param pb_floor should age in days be rounded down
#'
#' @return age in days
#'
#' @export age_in_days
age_in_days <- function(pdate_birth,
                        pdate_today = lubridate::today(),
                        pb_floor    = TRUE){

  result_age <- lubridate::interval(start = pdate_birth, end = pdate_today) / lubridate::duration(num = 1, units = "days")

  if (pb_floor){
    return(as.integer(floor(result_age)))
  }
  return(result_age)
}


#' @title Read file with input about beef recording for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will read a file with value coming from weights.
#' In this weight-file some information about ...
#'
#' @param ps_input_file_flp path to file with input coming from beef recording for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the beef recording date to filter in the flp-data
#' @param ps_end_flp_date setting the end of the beef recording date to filter in the flp-data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return tibble with the content of the beef recording file
#'
#' @export read_file_input_flp
read_file_input_flp <-  function(ps_input_file_flp,
                                 ps_start_flp_date,
                                 ps_end_flp_date,
                                 ps_sirebreed,
                                 pb_log = FALSE,
                                 plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'read_file_input_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'read_file_input_flp',
                    paste0('Starting function with parameters:\n * ps_input_file_flp: ', ps_input_file_flp,'\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n',
                           ' * ps_sirebreed: ',ps_sirebreed))
  }


  ### # Check if file exist otherwise stop running the function
  if(!file.exists(ps_input_file_flp)){
    stop("read_file_input_flp: file ",ps_input_file_flp," does not exist, please check the path !")
  }else{
    qp4ewc_log_info(lgr, 'read_file_input_flp',paste0('File exists:\n * ps_input_file_flp',ps_input_file_flp))
  }


  ### # Read the input flp file
  tbl_input <- readr::read_delim(file = ps_input_file_flp, delim = ";")
  qp4ewc_log_info(lgr, 'read_file_input_flp',paste0('Read file: \n * ps_input_file_flp: ',ps_input_file_flp,"\n",
                                                    ' * in a tibble'))


  ### # Check if some columns-header are available in the input flp file
  vec_flpHeader_name <- names(tbl_input)
  vec_requested_flpHeader_name <- c("Schlachtdatum","Geburtsdatum Nako","Nako RaceRode","Schlacht-/Masttierkategorie","Markenprogramm",
                                        "Geburtsgewicht Nako","Absetzgewicht effektiv","Absetzdatum Nako","Schlachtgewicht Nako","Laktationsnummer Ammen-Mutter")
  if(all(vec_requested_flpHeader_name %in% vec_flpHeader_name)){
    qp4ewc_log_info(lgr, 'read_file_input_flp',paste0('All requested column-names in flp input file exist'))
  }else{
    stop("read_file_input_flp: Not all requested column-names in flp input file exist, please check the file !")
  }


  ### # Selection criteria on the input flp file
  ### # Specific date interval to consider in the data
  tbl_input <- tbl_input %>% dplyr::filter(Schlachtdatum >= ps_start_flp_date) %>% dplyr::filter(Schlachtdatum <= ps_end_flp_date)
  qp4ewc_log_info(lgr, 'read_file_input_flp',paste0('Considered data from input flp file from: ',ps_start_flp_date, ' to ', ps_end_flp_date))
  ### # Consider specific breed in the data
  tbl_input <- tbl_input %>% dplyr::filter(`Nako RaceRode` == ps_sirebreed)
  qp4ewc_log_info(lgr, 'read_file_input_flp',paste0('Considered data from input flp file from the breed: ',ps_sirebreed))
  ### # Calculate age at slaughter in days
  tbl_input$ageAtSlaughterInDays <- age_in_days(pdate_birth = as.Date(as.character(as.numeric(tbl_input$`Geburtsdatum Nako`)), format = "%Y%m%d", origin="1970-01-01"),
                                                pdate_today = as.Date(as.character(as.numeric(tbl_input$Schlachtdatum)), format = "%Y%m%d", origin="1970-01-01"),
                                                pb_floor = FALSE)


  ### # Return tibble
  return(tbl_input)

}


