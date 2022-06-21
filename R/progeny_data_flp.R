### #
### #
### #
### #   Purpose:   Function related to the flp input file
### #   started:   2022-04-06 (skn)
### #
### # ##################################################################### ###


#' @title Calculate mean birthweight
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate mean birth weight
#' based on slaughtercategory, marketing-channel (label of Swiss Beef Cattle Assiocation) and sex.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_sex statement of sex ("F" for female and "M" for male)
#' @param ps_marketing_channel statement of marketing-channel for Natura-Beef (==2), SwissPrimBeef(==3)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return mean_birthweight vector
#'
#' @export calculate_mean_birthweight
calculate_mean_birthweight <- function(ps_input_flp_tibble,
                                       ps_sex,
                                       ps_marketing_channel,
                                       pb_log = FALSE,
                                       plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_mean_birthweight.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble \n',
                           ' * ps_sex: ', ps_sex,'\n',
                           ' * ps_marketing_channel: ',ps_marketing_channel,'\n'))
  }


  ### # Different tibble depending on ps_sex and ps_marketing_channel
  ### # Slaughtercategory for female to consider is RG == 5
  if(ps_sex == "F"){
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
                                         dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
                                         dplyr::select(`Geburtsgewicht Nako`,`ageAtSlaughterInDays`) %>%
                                         na.omit() %>%
                                         tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                      paste0('A Tibble for female has been created for the calculation of mean birthweight '))
    }
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
                                         dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
                                         dplyr::select(`Geburtsgewicht Nako`,`ageAtSlaughterInDays`) %>%
                                         na.omit() %>%
                                         tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                      paste0('A Tibble for male has been created for the calculation of mean birthweight '))
    }
  }


  ### # Calculate the mean birthweight
  birthwt <- round(as.numeric(dplyr::summarise(tbl_input, mean_birthwt = mean(`Geburtsgewicht Nako`))),4)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                    paste0('Mean birthweight for ',ps_sex,' is : ',birthwt))
  }

  return(birthwt)


}


#' @title Calculate mean live weight at slaughter
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate mean live weight at slaughter
#' based on slaughtercategory, marketing-channel (label of Swiss Beef Cattle Assiocation) and sex.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_sex statement of sex ("F" for female and "M" for male)
#' @param ps_marketing_channel statement of marketing-channel for Natura-Beef (==2), SwissPrimBeef(==3)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return livewt_atslaughter vector
#'
#' @export calculate_mean_liveweight_slaughter
calculate_mean_liveweight_slaughter <- function(ps_input_flp_tibble,
                                                 ps_sex,
                                                 ps_marketing_channel,
                                                 pb_log = FALSE,
                                                 plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_mean_liveweight_slaughter.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_mean_liveweight_slaughter',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble \n',
                           ' * ps_sex: ', ps_sex,'\n',
                           ' * ps_marketing_channel: ',ps_marketing_channel,'\n'))
  }


  ### # Different tibble depending on ps_sex and ps_marketing_channel
  ### # Slaughtercategory for female to consider is RG == 5
  if(ps_sex == "F"){
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
                                         dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
                                         dplyr::select(`Schlachtgewicht Nako`,ageAtSlaughterInDays) %>%
                                         na.omit() %>%
                                         tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_liveweight_slaughter',
                      paste0('A Tibble for female has been created for the calculation of mean live weight at slaughter '))
    }
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
                                         dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
                                         dplyr::select(`Schlachtgewicht Nako`,ageAtSlaughterInDays) %>%
                                         na.omit() %>%
                                         tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_liveweight_slaughter',
                      paste0('A Tibble for male has been created for the calculation of mean live weight at slaughter '))
    }
  }


  ### # Calculate the mean carcass weight
  carcasswt <- round(as.numeric(dplyr::summarise(tbl_input, mean_carcasswt = mean(`Schlachtgewicht Nako`))),4)


  ### # Get the constants
  l_constants <- get_constants()


  ### # Calculate the mean live weight at slaughter
  if(ps_sex == "F"){
    # dressing percentage to convert carcass weight to live weight at slaughter come from Proviande Wochenpreise für Rindvieh (RG, Fleischigkeit C)
    livewt_atslaughter <- round(as.numeric((carcasswt/l_constants$vec_dressing_female),4))
  }else{
    # dressing percentage to convert carcass weight to live weight at slaughter come from Proviande Wochenpreise für Rindvieh (MT, Fleischigkeit C)
    livewt_atslaughter <- round(as.numeric((carcasswt/l_constants$vec_dressing_male),4))
  }


  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_mean_liveweight_slaughter',
                    paste0('Mean live weight at slaughter for ',ps_sex,' is : ',livewt_atslaughter, ' based on mean carcass weight: ',carcasswt))
  }


  return(livewt_atslaughter)


}


#' @title Calculate mean weaning weight
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate mean weaning weight
#' based on slaughtercategory, marketing-channel (label of Swiss Beef Cattle Assiocation) and sex.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_sex statement of sex ("F" for female and "M" for male)
#' @param ps_marketing_channel statement of marketing-channel for Natura-Beef (==2), SwissPrimBeef(==3)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return weaningwt vector
#'
#' @export calculate_mean_weaningweight
calculate_mean_weaningweight <- function(ps_input_flp_tibble,
                                         ps_sex,
                                         ps_marketing_channel,
                                         pb_log = FALSE,
                                         plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_mean_weaningweight.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_mean_weaningweight',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble \n',
                           ' * ps_sex: ', ps_sex,'\n',
                           ' * ps_marketing_channel: ',ps_marketing_channel,'\n'))
  }


  ### # Different tibble depending on ps_sex and ps_marketing_channel
  ### # Slaughtercategory for female to consider is RG == 5
  if(ps_sex == "F"){
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
                                         dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
                                         dplyr::select(`Absetzgewicht effektiv`) %>%
                                         na.omit() %>%
                                         tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_weaningweight',
                      paste0('A Tibble for female has been created for the calculation of mean weaning weight '))
    }
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
                                         dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
                                         dplyr::select(`Absetzgewicht effektiv`) %>%
                                         na.omit() %>%
                                         tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_weaningweight',
                      paste0('A Tibble for male has been created for the calculation of mean weaning weight '))
    }
  }


  ### # Calculate mean weaning weight
  weaningwt <- round(as.numeric(dplyr::summarise(tbl_input, mean_weaningwt = mean(`Absetzgewicht effektiv`))),4)


  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_mean_weaningweight',
                    paste0('Mean weaning weight for ',ps_sex,' is : ',weaningwt))
  }


  return(weaningwt)


}


#' @title Calculate mean weaning age
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate mean weaning age
#' based on slaughtercategory, marketing-channel (label of Swiss Beef Cattle Assiocation) and sex.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_sex statement of sex ("F" for female and "M" for male)
#' @param ps_marketing_channel statement of marketing-channel for Natura-Beef (==2), SwissPrimBeef(==3)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return weaningage vector
#'
#' @export calculate_mean_weaningage
calculate_mean_weaningage <- function(ps_input_flp_tibble,
                                      ps_sex,
                                      ps_marketing_channel,
                                      pb_log = FALSE,
                                      plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_mean_weaningage.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_mean_weaningage',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble \n',
                           ' * ps_sex: ', ps_sex,'\n',
                           ' * ps_marketing_channel: ',ps_marketing_channel,'\n'))
  }


  ### # Different tibble depending on ps_sex and ps_marketing_channel
  ### # Slaughtercategory for female to consider is RG == 5
  if(ps_sex == "F"){
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
      dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
      dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
      dplyr::select(ageAtWeaningInDays) %>%
      na.omit() %>%
      tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_weaningage',
                      paste0('A Tibble for female has been created for the calculation of mean weaning age '))
    }
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
      dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
      dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
      dplyr::select(ageAtWeaningInDays) %>%
      na.omit() %>%
      tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_weaningage',
                      paste0('A Tibble for male has been created for the calculation of mean weaning age '))
    }
  }


  ### # Calculate mean weaning age
  weaningage <- round(as.numeric(dplyr::summarise(tbl_input, mean_weaningage = mean(ageAtWeaningInDays))),4)


  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_mean_weaningage',
                    paste0('Mean weaning age for ',ps_sex,' is : ',weaningage))
  }


  return(weaningage)


}


#' @title Calculate mean slaughter age
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate mean slaughter age
#' based on slaughtercategory, marketing-channel (label of Swiss Beef Cattle Assiocation) and sex.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_sex statement of sex ("F" for female and "M" for male)
#' @param ps_marketing_channel statement of marketing-channel for Natura-Beef (==2), SwissPrimBeef(==3)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return slaughterage vector
#'
#' @export calculate_mean_slaughterage
calculate_mean_slaughterage <- function(ps_input_flp_tibble,
                                        ps_sex,
                                        ps_marketing_channel,
                                        pb_log = FALSE,
                                        plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_mean_slaughterage.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_mean_slaughterage',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble \n',
                           ' * ps_sex: ', ps_sex,'\n',
                           ' * ps_marketing_channel: ',ps_marketing_channel,'\n'))
  }


  ### # Different tibble depending on ps_sex and ps_marketing_channel
  ### # Slaughtercategory for female to consider is RG == 5
  if(ps_sex == "F"){
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
      dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
      dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
      dplyr::select(`Geburtsgewicht Nako`,ageAtSlaughterInDays) %>%
      na.omit() %>%
      dplyr::select(ageAtSlaughterInDays) %>%
      tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_slaughterage',
                      paste0('A Tibble for female has been created for the calculation of mean slaughter age '))
    }
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
      dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
      dplyr::filter(Markenprogramm == ps_marketing_channel) %>%
      dplyr::select(`Geburtsgewicht Nako`,ageAtSlaughterInDays) %>%
      na.omit() %>%
      dplyr::select(ageAtSlaughterInDays) %>%
      tidyr::drop_na()
    if(pb_log){
      qp4ewc_log_info(lgr, 'calculate_mean_slaughterage',
                      paste0('A Tibble for male has been created for the calculation of mean slaughter age '))
    }
  }


  ### # Calculate mean slaughter age
  slaughterage <- round(as.numeric(dplyr::summarise(tbl_input, mean_slaughterage = mean(ageAtSlaughterInDays))),4)


  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_mean_slaughterage',
                    paste0('Mean slaughter age for ',ps_sex,' is : ',slaughterage))
  }


  return(slaughterage)


}


#' @title Calculate average daily gain
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate average daily gain.
#'
#' @param pv_mean_slaughterage vector with the mean of slaughter age
#' @param pv_mean_weaningage vector with the mean of weaning age
#' @param pv_mean_livewt_atslaughter vector with the mean of live weight at slaughter
#' @param pv_mean_weaningwt vector with the mean of weaning weight
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return dailygain vector
#'
#' @export calculate_dailygain
calculate_dailygain <- function(pv_mean_slaughterage,
                                pv_mean_weaningage,
                                pv_mean_livewt_atslaughter,
                                pv_mean_weaningwt,
                                pb_log = FALSE,
                                plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_dailygain.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_dailygain',
                    paste0('Starting function with parameters:\n * pv_mean_slaughterage: ',pv_mean_slaughterage,'\n',
                           ' * pv_mean_weaningage: ', pv_mean_weaningage,'\n',
                           ' * pv_mean_livewt_atslaughter: ', pv_mean_livewt_atslaughter,'\n',
                           ' * pv_mean_weaningwt: ', pv_mean_weaningwt))
  }


  ### # Calculate fattening days
  fattening_days <- as.numeric(pv_mean_slaughterage - pv_mean_weaningage)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_dailygain',
                    paste0('Fattening days: ',fattening_days,' is the difference between pv_mean_slaughterage ',pv_mean_slaughterage,' and pv_mean_weaningage ', pv_mean_weaningage))
  }


  ### # Calculate fattening weight
  fattening_weight <- pv_mean_livewt_atslaughter - pv_mean_weaningwt
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_dailygain',
                    paste0('Fattening weight: ',fattening_weight,' is the difference between pv_mean_livewt_atslaughter ',pv_mean_livewt_atslaughter,' and pv_mean_weaningwt ', pv_mean_weaningwt))
  }


  ### # Calculate daily gain
  dailygain <- round(fattening_weight/fattening_days,4)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_dailygain',
                    paste0('Daily gain during fattening is : ',dailygain))
  }


  return(dailygain)


}


#' @title Extrapolate weaning weight at t-days
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate the extrapolate weaning weight
#' at t-days.
#'
#' @param pv_mean_weaningage vector with the mean of weaning age
#' @param pv_daily_gain vector with daily gain
#' @param pv_mean_weaningwt vector with the mean of weaning weight
#' @param pv_t_days vector for t-days
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return weaningweight_tdays vector
#'
#' @export calculate_extrapolated_weaningweight
calculate_extrapolated_weaningweight <- function(pv_mean_weaningage,
                                                 pv_daily_gain,
                                                 pv_mean_weaningwt,
                                                 pv_t_days,
                                                 pb_log = FALSE,
                                                 plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_extrapolated_weaningweight.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_extrapolated_weaningweight',
                    paste0('Starting function with parameters:\n * pv_mean_weaningage: ',pv_mean_weaningage,'\n',
                           ' * pv_daily_gain: ', pv_daily_gain,'\n',
                           ' * pv_mean_weaningwt: ', pv_mean_weaningwt,'\n',
                           ' * pv_t_days: ', pv_t_days))
  }


  ### # Calculate extrapolated weaning days
  extrapolatedweaning_days <- as.numeric(pv_t_days - pv_mean_weaningage)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_extrapolated_weaningweight',
                    paste0('Extrapolated weaning days: ',extrapolatedweaning_days,' is the difference between pv_t_days ',pv_t_days,' and pv_mean_weaningage ', pv_mean_weaningage))
  }


  ### # Extrapolated weaning weight
  extrapolatedweaning_weight <- pv_mean_weaningwt + (extrapolatedweaning_days * pv_daily_gain)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_extrapolated_weaningweight',
                    paste0('Extrapolated weaning weight: ',extrapolatedweaning_weight,' is based on pv_mean_weaningwt ',pv_mean_weaningwt,' , extrapolatedweaning_days ', extrapolatedweaning_days, ' and pv_daily_gain ',pv_daily_gain))
  }


  extrapolatedweaning_weight <- round(as.numeric(extrapolatedweaning_weight),2)
  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_extrapolated_weaningweight',
                    paste0('Extrapolated weaning weight is : ',extrapolatedweaning_weight))
  }


  return(extrapolatedweaning_weight)


}


#' @title Calculate cow live weight
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate cow live weight
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_first_calvingweight flag to calculate first calving weight (TRUE or FALSE)
#' @param ps_second_calvingweight flag to calculate second calving weight (TRUE or FALSE)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return cow_liveweight vector
#'
#' @export calculate_cow_liveweight
calculate_cow_liveweight <- function(ps_input_flp_tibble,
                                     ps_first_calvingweight,
                                     ps_second_calvingweight,
                                     pb_log = FALSE,
                                     plogger = NULL){


  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_cow_liveweight.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_cow_liveweight',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble: \n',
                           ' * ps_second_calvingweight: ', ps_second_calvingweight))
  }


  ### # Tibble depending on ps_second_calvingweight
  ### # Calculate cow weight after first calving (this mean below 2nd lactation number)
  if(ps_first_calvingweight){
    ### # Slaughtercategory for cow to consider is VK == 7
    tbl_input <- ps_input_flp_tibble %>%
                 dplyr::filter(`Schlacht-/Masttierkategorie` == 7) %>%
                 dplyr::filter(`Laktationsnummer Ammen-Mutter` < 2) %>%
                 dplyr::select(`Schlachtgewicht Nako`,`Geburtsdatum Nako`,Schlachtdatum)  %>%
                 tidyr::drop_na()
  ### # Calculate cow weight after second calving (this mean below 4th lactation number)
  }else if(ps_second_calvingweight){
    tbl_input <- ps_input_flp_tibble %>%
                 dplyr::filter(`Schlacht-/Masttierkategorie` == 7) %>%
                 dplyr::filter(`Laktationsnummer Ammen-Mutter` < 4) %>%
                 dplyr::select(`Schlachtgewicht Nako`,`Geburtsdatum Nako`,Schlachtdatum)  %>%
                 tidyr::drop_na()
  }else{
    ### # Calculate mature cow weight
    tbl_input <- ps_input_flp_tibble %>%
                 dplyr::filter(`Schlacht-/Masttierkategorie` == 7) %>%
                 dplyr::filter(`Laktationsnummer Ammen-Mutter` > 3) %>%
                 dplyr::filter(ageAtSlaughterInDays > 1460) %>%
                 dplyr::select(`Schlachtgewicht Nako`,`Geburtsdatum Nako`,Schlachtdatum)  %>%
                 tidyr::drop_na()
  }


   ### # Calculate mean carcass weight for cow
   cowwt <- round(as.numeric(dplyr::summarise(tbl_input, mean_cowwt = mean(`Schlachtgewicht Nako`))),4)


   ### # Get the constants
   l_constants <- get_constants()


   ### # Calculate the mean cow live weight at slaughter
   # dressing percentage to convert carcass weight to cow live weight at slaughter come from Proviande Wochenpreise für Rindvieh (VK, Fleischigkeit C)
   cowlivewt_atslaughter <- round(as.numeric((cowwt/l_constants$vec_dressing_female),4))


   if(pb_log){
     qp4ewc_log_info(lgr, 'calculate_cow_liveweight',
                     paste0('Mean cow live weight at slaughter is : ',cowlivewt_atslaughter, ' based on mean cow carcass weight: ',cowwt))
   }


   return(cowlivewt_atslaughter)


}


#' @title Calculate bull mature live weight
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will calculate bull live weight.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return bull_liveweight vector
#'
#' @export calculate_bull_liveweight
calculate_bull_liveweight <- function(ps_input_flp_tibble,
                                     pb_log = FALSE,
                                     plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'calculate_bull_liveweight.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'calculate_bull_liveweight',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble'))
  }


  ### # Calculate bull mature weight
  ### # Slaughtercategory for older bull to consider is MA == 4
  tbl_input <- ps_input_flp_tibble %>%
               dplyr::filter(`Schlacht-/Masttierkategorie` == 4) %>%
               dplyr::filter(ageAtSlaughterInDays > 1460) %>%
               dplyr::select(`Schlachtgewicht Nako`) %>%
               tidyr::drop_na()


  ### # Calculate mean carcass weight for bull
  bullwt <- round(as.numeric(dplyr::summarise(tbl_input, mean_bullwt = mean(`Schlachtgewicht Nako`))),4)


  ### # Get the constants
  l_constants <- get_constants()


  ### # Calculate the mean bull live weight at slaughter
  # dressing percentage to convert carcass weight to bull live weight at slaughter come from Proviande Wochenpreise für Rindvieh (MA, Fleischigkeit C)
  bulllivewt_atslaughter <- round(as.numeric((bullwt/l_constants$vec_dressing_male),4))


  if(pb_log){
    qp4ewc_log_info(lgr, 'calculate_bull_liveweight',
                    paste0('Mean bull live weight at slaughter is : ',bulllivewt_atslaughter, ' based on mean bull carcass weight: ',bullwt))
  }


  return(bulllivewt_atslaughter)


}
