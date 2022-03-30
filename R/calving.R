### #
### #
### #
### #   Purpose:   Function related to the calving input file
### #   started:   2022-03-28 (skn)
### #
### # ##################################################################### ###


#' @title Calculate abortion rate
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate based on calving scores as well
#' as lactation number of the dam (primiparous vs multiparous) the abortion rate.
#'
#' @param ps_input_calving_tibble input calving tibble coming from read_file_input_calving in this package
#' @param ps_statement_firstlactation statement if in first lactation status (TRUE or FALSE)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return abortion_rate vector
#'
#' @export calculate_abortion_rate
calculate_abortion_rate <- function(ps_input_calving_tibble,
                                    ps_statement_firstlactation = TRUE,
                                    pb_log = FALSE,
                                    plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_abortion_rate.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('Starting function with parameters:\n * ps_input_calving_tibble \n',
                           ' * ps_statement_firstlactation: ', ps_statement_firstlactation))
  }


  ### # Consider only known calving score
  tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf != 0)
  qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                  paste0('Only consider known calving score to calculate abortion rate'))


  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_abort <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == 1) %>%
                               dplyr::select(Abort) %>%
                                na.omit() %>%
                               dplyr::group_by(Abort) %>%
                               dplyr::count()

    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('A Tibble for primiparous has been created for the calculation of abortion rate '))

  }else{
    tbl_abort <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter > 1) %>%
                               dplyr::select(Abort) %>%
                               na.omit() %>%
                               dplyr::group_by(Abort) %>%
                               dplyr::count()

    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('A Tibble for multiparous has been created for the calculation of abortion rate '))
  }


  ### # The value in case of an abort in the data is 1
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for abort are available to calculate abortion rate
  if(nrow(tbl_abort %>% dplyr::filter(Abort == 1)) != 0){
    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('Abort information are available in the dataset so that abortion rate can be calculated'))
  }else{
    stop("calculate_abortion_rate: no abort information are available in the dataset, please check the dataset !")
  }


  ### # Add frequence according to abort in a vector
  abort_freq <- tbl_abort %>% dplyr::filter(Abort == 1) %>% dplyr::pull(n)
  sum_abort_freq <- sum(tbl_abort$n)


  ### # Calculate abortion rate
  abortion_rate <- round(abort_freq/sum_abort_freq,4)
  qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                  paste0('abortion_rate is : ',abortion_rate))


  return(abortion_rate)

}



#' @title Calculate stillbirth rate
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate based on calving scores as well
#' as lactation number of the dam (primiparous vs multiparous) the stillbirth rate.
#'
#' @param ps_input_calving_tibble input calving tibble coming from read_file_input_calving in this package
#' @param ps_statement_firstlactation statement if in first lactation status (TRUE or FALSE)
#' @param ps_statement_easycalving statement if for easy calving status (TRUE or FALSE)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return stillbirth_rate vector
#'
#' @export calculate_stillbirth_rate
calculate_stillbirth_rate <- function(ps_input_calving_tibble,
                                      ps_statement_firstlactation = TRUE,
                                      ps_statement_easycalving = TRUE,
                                      pb_log = FALSE,
                                      plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_stillbirth_rate.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                    paste0('Starting function with parameters:\n * ps_input_calving_tibble \n',
                           ' * ps_statement_firstlactation: ', ps_statement_firstlactation,'\n',
                           ' * ps_statement_easycalving: ',ps_statement_easycalving))
  }


  ### # Different calculation depending on ps_statement_easycalving
  if(ps_statement_easycalving){
    ### # The calving score of 1 = without help or 2 = slight help are considered as easy calving
    ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
    tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == 1 | Geburtsverlauf == 2)
    qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                    paste0('A Tibble for easy calving has been created for the calculation of stillbirth rate '))
  }else{
    ### # The calving score of 3 = difficult or 4 = cesarean are considered as difficult calving
    tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == 3 | Geburtsverlauf == 4)
    qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                    paste0('A Tibble for difficult calving has been created for the calculation of stillbirth rate '))
  }

  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_stillbirth <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == 1) %>%
                                    dplyr::select(Code_TotOLebend) %>%
                                    na.omit() %>%
                                    dplyr::group_by(Code_TotOLebend) %>%
                                    dplyr::count()

    qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                    paste0('A Tibble for primiparous has been created for the calculation of stillbirth rate '))

  }else{
    tbl_stillbirth <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter > 1) %>%
                                    dplyr::select(Code_TotOLebend) %>%
                                    na.omit() %>%
                                    dplyr::group_by(Code_TotOLebend) %>%
                                    dplyr::count()

    qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                    paste0('A Tibble for multiparous has been created for the calculation of stillbirth rate '))
  }


  ### # The value in case of a stillbirth in the data is 4
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for stillbirth are available to calculate stillbirth rate
  if(nrow(tbl_stillbirth %>% dplyr::filter(Code_TotOLebend == 4)) != 0){
    qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                    paste0('Stillbirth information are available in the dataset so that stillbirth rate can be calculated'))
  }else{
    stop("calculate_stillbirth_rate: no stillbirth information are available in the dataset, please check the dataset !")
  }


  ### # Add frequence according to abort in a vector
  stillbirth_freq <- tbl_stillbirth %>% dplyr::filter(Code_TotOLebend == 4) %>% dplyr::pull(n)
  sum_stillbirth_freq <- sum(tbl_stillbirth$n)


  ### # Calculate stillbirth rate
  stillbirth_rate <- round(stillbirth_freq/sum_stillbirth_freq,4)
  qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                  paste0('stillbirth_rate is : ',stillbirth_rate))


  return(stillbirth_rate)

}

