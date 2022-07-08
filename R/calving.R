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
#' @import dplyr
#' @import tidyr
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
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('Only consider known calving score to calculate abortion rate'))
  }


  ### # Get the constants
  l_constants <- get_constants()


  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_abort <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == l_constants$lactnumb1) %>%
                               dplyr::select(Abort) %>%
                                na.omit() %>%
                               dplyr::group_by(Abort) %>%
                               dplyr::count() %>%
                               tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                      paste0('A Tibble for primiparous has been created for the calculation of abortion rate '))
    }

  }else{
    tbl_abort <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter > l_constants$lactnumb1) %>%
                               dplyr::select(Abort) %>%
                               na.omit() %>%
                               dplyr::group_by(Abort) %>%
                               dplyr::count() %>%
                               tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                      paste0('A Tibble for multiparous has been created for the calculation of abortion rate '))
    }
  }


  ### # The value in case of an abort in the data is 1
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for abort are available to calculate abortion rate
  if(nrow(tbl_abort %>% dplyr::filter(Abort == l_constants$abort_value)) != 0){
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                      paste0('Abort information are available in the dataset so that abortion rate can be calculated'))
    }

    ### # Add frequence according to abort in a vector
    abort_freq <- tbl_abort %>% dplyr::filter(Abort == l_constants$abort_value) %>% dplyr::pull(n)
    sum_abort_freq <- sum(tbl_abort$n)


    ### # Calculate abortion rate
    abortion_rate <- round(abort_freq/sum_abort_freq,4)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                      paste0('abortion_rate is : ',abortion_rate))
    }

  }else{
    warning("calculate_abortion_rate: no abort information are available in the dataset !")
    abortion_rate <- 0
  }


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
#' @import dplyr
#' @import tidyr
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


  ### # Get the constants
  l_constants <- get_constants()


  ### # Different calculation depending on ps_statement_easycalving
  if(ps_statement_easycalving){
    ### # The calving score of 1 = without help or 2 = slight help are considered as easy calving
    ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
    tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == l_constants$calf_score_nohelp | Geburtsverlauf == l_constants$calf_score_slighthelp)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('A Tibble for easy calving has been created for the calculation of stillbirth rate '))
    }
  }else{
    ### # The calving score of 3 = difficult or 4 = cesarean are considered as difficult calving
    tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == l_constants$calf_score_difficult | Geburtsverlauf == l_constants$calf_score_cesarean)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('A Tibble for difficult calving has been created for the calculation of stillbirth rate '))
    }
  }

  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_stillbirth <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == l_constants$lactnumb1) %>%
                                    dplyr::select(Code_TotOLebend) %>%
                                    na.omit() %>%
                                    dplyr::group_by(Code_TotOLebend) %>%
                                    dplyr::count() %>%
                                    tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('A Tibble for primiparous has been created for the calculation of stillbirth rate '))
    }

  }else{
    tbl_stillbirth <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter > l_constants$lactnumb1) %>%
                                    dplyr::select(Code_TotOLebend) %>%
                                    na.omit() %>%
                                    dplyr::group_by(Code_TotOLebend) %>%
                                    dplyr::count() %>%
                                    tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('A Tibble for multiparous has been created for the calculation of stillbirth rate '))
    }
  }


  ### # The value in case of a stillbirth in the data is 4
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for stillbirth are available to calculate stillbirth rate
  if(nrow(tbl_stillbirth %>% dplyr::filter(Code_TotOLebend == l_constants$stillbirth_value)) != 0){
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('Stillbirth information are available in the dataset so that stillbirth rate can be calculated'))
    }


    ### # Add frequence according to stillbirth in a vector
    stillbirth_freq <- tbl_stillbirth %>% dplyr::filter(Code_TotOLebend == l_constants$stillbirth_value) %>% dplyr::pull(n)
    sum_stillbirth_freq <- sum(tbl_stillbirth$n)


    ### # Calculate stillbirth rate
    stillbirth_rate <- round(stillbirth_freq/sum_stillbirth_freq,4)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('stillbirth_rate is : ',stillbirth_rate))
    }

  }else{
    warning("calculate_stillbirth_rate: no stillbirth information are available in the dataset !")
    stillbirth_rate <- 0

  }


  return(stillbirth_rate)

}



#' @title Calculate calving score proportions
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate based on breed, sex as well
#' as lactation number of the dam (primiparous vs multiparous) the calving score proportion.
#'
#' @param ps_input_calving_tibble input calving tibble coming from read_file_input_calving in this package
#' @param ps_statement_firstlactation statement if in first lactation status (TRUE or FALSE)
#' @param ps_sex set the sex (F, M)
#' @param ps_calvingscore set the calving score for which the proportion should be calculated (2,3,4)
#' @param ps_sirebreed set the breed (AN, AU, CH, LM, SI, OB)
#' @param ps_dambreed set the breed (AN, AU, CH, LM, SI, OB, HO, BS)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tidyr
#'
#' @return calving score proportion vector
#'
#' @export calculate_calvingscore_proportion
calculate_calvingscore_proportion <- function(ps_input_calving_tibble,
                                              ps_statement_firstlactation = TRUE,
                                              ps_sex,
                                              ps_calvingscore,
                                              ps_sirebreed,
                                              ps_dambreed,
                                              pb_log = FALSE,
                                              plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_calvingscore_proportion.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_calvingscore_proportion',
                    paste0('Starting function with parameters:\n * ps_input_calving_tibble \n',
                           ' * ps_statement_firstlactation: ', ps_statement_firstlactation,'\n',
                           ' * ps_sirebreed: ',ps_sirebreed,'\n',
                           ' * ps_dambreed: ',ps_dambreed,'\n',
                           ' * ps_sex: ',ps_sex,'\n',
                           ' * ps_calvingscore: ',ps_calvingscore))
  }


  ### # Filter criteria depending on ps_sirebreed and ps_sex
  tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>%
                                           dplyr::filter(Mutter_RasseCode == ps_dambreed) %>%
                                           dplyr::filter(Geschlecht == ps_sex)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_calvingscore_proportion',
                    paste0('A Tibble depending on the breed and sex has been created for the calculation of calving score proportion '))
  }


  ### # Get the constants
  l_constants <- get_constants()


  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_calvingprop <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == l_constants$lactnumb1) %>%
                                     dplyr::select(Geburtsverlauf) %>%
                                     na.omit() %>%
                                     dplyr::na_if(0) %>%
                                     dplyr::group_by(Geburtsverlauf) %>%
                                     dplyr::count() %>%
                                     tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvingscore_proportion',
                      paste0('A Tibble for primiparous has been created for the calculation of calving score proportion '))
    }
  }else{
    tbl_calvingprop <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter > l_constants$lactnumb1) %>%
                                     dplyr::select(Geburtsverlauf) %>%
                                     na.omit() %>%
                                     dplyr::na_if(0) %>%
                                     dplyr::group_by(Geburtsverlauf) %>%
                                     dplyr::count() %>%
                                     tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvingscore_proportion',
                      paste0('A Tibble for multiparous has been created for the calculation of calving score proportion '))
    }
  }


  ### # Calving score 2 = slight help, 3 = difficult, 4 = cesarean
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for specific calving score are available
  if(nrow(tbl_calvingprop %>% dplyr::filter(Geburtsverlauf == ps_calvingscore)) != 0){

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvingscore_proportion',
                      paste0('Calving score information are available in the dataset so that calving score proportion can be calculated'))
    }

    ### # Add frequence according to calving score in a vector
    calvingscore_freq <- tbl_calvingprop %>% dplyr::filter(Geburtsverlauf == ps_calvingscore) %>% dplyr::pull(n)
    sum_calvingscore_freq <- sum(tbl_calvingprop$n)


    ### # Calculate calving score proportion
    calvingscore_prop <- round(calvingscore_freq/sum_calvingscore_freq,4)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_stillbirth_rate',
                      paste0('calving score ',ps_calvingscore,' proportion is : ',calvingscore_prop))
    }


  }else{
    warning("calculate_calvingscore_proportion: no calving score information are available in the dataset!")
    calvingscore_prop <- 0
  }


  return(calvingscore_prop)


}



#' @title Calculate proportion of calves died to 24 hours
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate based on calving score,
#' lactation number of the dam (primiparous vs multiparous) to calculate the proportion of calves died.
#' The parameter in ECOWEIGHT specifies within 48 hours after birth. However, we only have data specified for up to 24hours after birth.
#' Therefore we use 24hours in this function.
#'
#' @param ps_input_calving_tibble input calving tibble coming from read_file_input_calving in this package
#' @param ps_statement_firstlactation statement if in first lactation status (TRUE or FALSE)
#' @param ps_statement_easycalving statement if for easy calving status (TRUE or FALSE)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tidyr
#'
#' @return calves died to 24 hours proportion vector
#'
#' @export calculate_calvesdied24h_proportion
calculate_calvesdied24h_proportion <- function(ps_input_calving_tibble,
                                               ps_statement_firstlactation = TRUE,
                                               ps_statement_easycalving = TRUE,
                                               pb_log = FALSE,
                                               plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_calvesdied24h_proportion',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                    paste0('Starting function with parameters:\n * ps_input_calving_tibble \n',
                           ' * ps_statement_firstlactation: ', ps_statement_firstlactation,'\n',
                           ' * ps_statement_easycalving: ',ps_statement_easycalving))
  }


  ### # Get the constants
  l_constants <- get_constants()


  ### # Different calculation depending on ps_statement_easycalving
  if(ps_statement_easycalving){
    ### # The calving score of 1 = without help or 2 = slight help are considered as easy calving
    ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
    tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == l_constants$calf_score_nohelp | Geburtsverlauf == l_constants$calf_score_slighthelp)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                      paste0('A Tibble for easy calving has been created for the calculation of proportion of calves died 24h'))
    }
  }else{
    ### # The calving score of 3 = difficult or 4 = cesarean are considered as difficult calving
    tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == l_constants$calf_score_difficult | Geburtsverlauf == l_constants$calf_score_cesarean)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                      paste0('A Tibble for difficult calving has been created for the calculation of proportion of calves died 24h'))
    }
  }


  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_calvesdied24h <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == l_constants$lactnumb1) %>%
                                       dplyr::select(Code_TotOLebend) %>%
                                       dplyr::na_if(0) %>%
                                       dplyr::group_by(Code_TotOLebend) %>%
                                       dplyr::count() %>%
                                       tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                      paste0('A Tibble for primiparous has been created for the calculation of proportion of calves died 24h'))
    }

  }else{
    tbl_calvesdied24h <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter > l_constants$lactnumb1) %>%
                                       dplyr::select(Code_TotOLebend) %>%
                                       dplyr::na_if(0) %>%
                                       dplyr::group_by(Code_TotOLebend) %>%
                                       dplyr::count() %>%
                                       tidyr::drop_na()

    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                      paste0('A Tibble for multiparous has been created for the calculation of proportion of calves died 24h'))
    }
  }


  ### # The value in case of a stillbirth within 24 hours is 2
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for stillbirth within 24 hours are available to calculate the proportion
  if(nrow(tbl_calvesdied24h %>% dplyr::filter(Code_TotOLebend == l_constants$stillbirth_within24h)) != 0){
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                      paste0('Stillbirth within 24h information are available in the dataset so that proportion of calves died 24h can be calculated'))
    }


    ### # Add frequence in a vector
    calvdied24h_freq <- tbl_calvesdied24h %>% dplyr::filter(Code_TotOLebend == l_constants$stillbirth_within24h) %>% dplyr::pull(n)
    sum_calvdied24h_freq <- sum(tbl_calvesdied24h$n)


    ### # Calculate proportion
    calvingdied24h_prop <- round(calvdied24h_freq/sum_calvdied24h_freq,4)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdied24h_proportion',
                      paste0('calves died in 24h proportion is : ',calvingdied24h_prop))
    }

  }else{
    warning("calculate_calvesdied24h_proportion: no stillbirth within 24h information are available in the dataset!")
    calvingdied24h_prop <- 0
  }


  return(calvingdied24h_prop)


}


#' @title Calculate proportion of calves died after 24 hours
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate the proportion of calves died after 24h.
#' It would be nice to have the information after 48h to weaning. But at this stage, we don't.
#'
#' @param ps_input_calving_tibble input calving tibble coming from read_file_input_calving in this package
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tidyr
#'
#' @return calves died after 24 hours proportion vector
#'
#' @export calculate_calvesdiedafter24h_proportion
calculate_calvesdiedafter24h_proportion <- function(ps_input_calving_tibble,
                                               pb_log = FALSE,
                                               plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_calvesdiedafter24h_proportion',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_calvesdiedafter24h_proportion',
                    paste0('Starting function with parameters:\n * ps_input_calving_tibble'))
  }


  ### # Build a tibble for the calculation of proportion of calves died after 24h
  tbl_calvesdiedafter24h <- ps_input_calving_tibble %>% dplyr::select(Code_TotOLebend) %>%
                            dplyr::na_if(0) %>%
                            dplyr::group_by(Code_TotOLebend) %>%
                            dplyr::count() %>%
                            tidyr::drop_na()

  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_calvesdiedafter24h_proportion',
                    paste0('A Tibble has been created for the calculation of proportion of calves died after 24h'))
  }


  ### # Get the constants
  l_constants <- get_constants()


  ### # The value in case of a stillbirth over 24 hours is 3
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for stillbirth over 24 hours are available to calculate the proportion
  if(nrow(tbl_calvesdiedafter24h %>% dplyr::filter(Code_TotOLebend == l_constants$stillbirth_over24h)) != 0){
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdiedafter24h_proportion',
                      paste0('Stillbirth over 24h information are available in the dataset so that proportion of calves died after 24h can be calculated'))
    }


    ### # Add frequence in a vector
    calvdiedafter24h_freq <- tbl_calvesdiedafter24h %>% dplyr::filter(Code_TotOLebend == l_constants$stillbirth_over24h) %>% dplyr::pull(n)
    sum_calvdiedafter24h_freq <- sum(tbl_calvesdiedafter24h$n)


    ### # Calculate proportion
    calvingdiedafter24h_prop <- round(calvdiedafter24h_freq/sum_calvdiedafter24h_freq,4)
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_calvesdiedafter24h_proportion',
                      paste0('calves died after 24h proportion is : ',calvingdiedafter24h_prop))
    }

  }else{
    warning("calculate_calvesdiedafter24h_proportion: no stillbirth over 24h information are available in the dataset!")
    calvingdiedafter24h_prop <- 0
  }


  return(calvingdiedafter24h_prop)


}
