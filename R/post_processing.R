### #
### #
### #
### #   Purpose:   Function related to the post-processing steps
### #   started:   2022-05-20 (skn)
### #
### # ##################################################################### ###


#' @title Post-processing the output-parameter-file of ECOWEIGHT beef cattle
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to extract information from the output parameter files.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export post_process_ewbc_output
post_process_ewbc_output <- function(ps_path_2outputfile,
                                     ps_output_statement,
                                     ps_output_search_pattern,
                                     pb_log,
                                     plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'post_process_ewbc_output.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'post_process_ewbc_output',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile))
  }


  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- qp4ewc::read_file_input(ps_output_statement,
                                                  pb_log,
                                                  plogger = lgr)


  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log,
                                plogger = lgr)



  # ****************************************************************************
  ## ---- Economic weight results ----
  # ****************************************************************************
  ### # Extract the part of interest of the results coming from ECOWEIGHT output
  vec_ecow_result_EW <- extract_result(ps_path_2outputfile,
                                       ps_start_statement2extract = tbl_output_statement[1,],
                                       ps_end_statement2extract = tbl_output_statement[2,],
                                       pb_log,
                                       plogger = lgr)


  ### # Get the value
  tbl_result_ew <- NULL
  for(idx in 1:nrow(tbl_search[1:15,])){
    n_cur_ew <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_EW,
                                 ps_statement2search = tbl_search$SearchPattern[idx],
                                 ps_line2get = tbl_search$IndexOffset[idx],
                                 ps_splitby = ":",
                                 pb_log,
                                 plogger = lgr)
    n_cur_ew <- as.numeric(n_cur_ew[2])
    tbl_cur_ew <- tibble::tibble(Trait = tbl_search$Trait[idx], EconomicValue = n_cur_ew)
    if (is.null(tbl_result_ew)){
      tbl_result_ew <- tbl_cur_ew
    } else {
      tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_cur_ew)
    }
  }


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for weaning weight direct and maternal
  vec_ecow_result_weaning <- extract_result(ps_path_2outputfile,
                                           ps_start_statement2extract = tbl_output_statement[2,],
                                           ps_end_statement2extract = tbl_output_statement[3,],
                                           pb_log,
                                           plogger = lgr)
  ### # Get the economic value for weaning weight direct and maternal
  vec_cur_ew_weaning <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_weaning,
                                         ps_statement2search = tbl_search$SearchPattern[16],
                                         ps_line2get = tbl_search$IndexOffset[16],
                                         ps_splitby = "       ",
                                         pb_log,
                                         plogger = lgr)
  n_cur_ew_weaning_direct <- as.numeric(vec_cur_ew_weaning[1])
  n_cur_ew_weaning_maternal <- as.numeric(vec_cur_ew_weaning[2])
  ### # Put the value in a result tibble for economic weight
  tbl_cur_ew_weaning <- tibble::tibble(Trait = c(tbl_search$Trait[16],
                                                 tbl_search$Trait[17]),
                                       EconomicValue = c(n_cur_ew_weaning_direct,
                                                         n_cur_ew_weaning_maternal))
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_cur_ew_weaning)


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for fattening length
  ### # Average fattening length is required for calculating the economic weight for age adjusted carcass weight: EW ADG/days fattening
  vec_ecow_result_fat <- extract_result(ps_path_2outputfile,
                                        ps_start_statement2extract = tbl_output_statement[4,],
                                        ps_end_statement2extract = tbl_output_statement[5,],
                                        pb_log,
                                        plogger = lgr)
  tbl_result_ew_fat <- NULL
  tbl_search_fat <- tbl_search[18:19,]
  for(idx in 1:nrow(tbl_search_fat)){
    n_cur_ew_fat <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat,
                                     ps_statement2search = tbl_search_fat$SearchPattern[idx],
                                     ps_line2get = tbl_search_fat$IndexOffset[idx],
                                     ps_splitby = "    ",
                                     pb_log,
                                     plogger = lgr)
    n_cur_ew_fat <- as.numeric(n_cur_ew_fat[4])
    tbl_cur_ew_fat <- tibble::tibble(Trait = tbl_search_fat$Trait[idx], EconomicValue = n_cur_ew_fat)
    if (is.null(tbl_result_ew_fat)){
      tbl_result_ew_fat <- tbl_cur_ew_fat
    } else {
      tbl_result_ew_fat <- dplyr::bind_rows(tbl_result_ew_fat, tbl_cur_ew_fat)
    }
  }
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_result_ew_fat)


  avg_fattening_length <- sum(tbl_result_ew_fat$EconomicValue)/2


  # ****************************************************************************
  ## ---- Average values ----
  # ****************************************************************************
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for proportion of males and females
  ### # How many females are kept for replacement will affect how to calculate the average trait values at slaughter
  vec_ecow_result_prop <- extract_result(ps_path_2outputfile,
                                        ps_start_statement2extract = tbl_output_statement[6,],
                                        ps_end_statement2extract = tbl_output_statement[7,],
                                        pb_log,
                                        plogger = lgr)
  tbl_result_prop <- NULL
  tbl_search_prop <- tbl_search[20:21,]
  for (idx in 1:nrow(tbl_search_prop)){
    n_cur_prop <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_prop,
                                     ps_statement2search = tbl_search_prop$SearchPattern[idx],
                                     ps_line2get = tbl_search_prop$IndexOffset[idx],
                                     ps_splitby = ":",
                                     pb_log,
                                     plogger = lgr)
    n_cur_prop <- as.numeric(n_cur_prop[2])
    tbl_cur_result_prop <- tibble::tibble(Trait = tbl_search_prop$Trait[idx], MeanValue = n_cur_prop)
    if (is.null(tbl_result_prop)){
      tbl_result_prop <- tbl_cur_result_prop
    } else {
      tbl_result_prop <- dplyr::bind_rows(tbl_result_prop, tbl_cur_result_prop)
    }
  }


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for calving performance, fleshiness, fat
  vec_ecow_result_misc <- extract_result(ps_path_2outputfile,
                                         ps_start_statement2extract = tbl_output_statement[8,],
                                         ps_end_statement2extract = tbl_output_statement[9,],
                                         pb_log,
                                         plogger = lgr)
  tbl_result_misc <- NULL
  tbl_search_misc <- tbl_search[22:28,]
  for (idx in 1:nrow(tbl_search_misc)){
    n_cur_misc <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_misc,
                                   ps_statement2search = tbl_search_misc$SearchPattern[idx],
                                   ps_line2get = tbl_search_misc$IndexOffset[idx],
                                   ps_splitby = ":",
                                   pb_log,
                                   plogger = lgr)
    n_cur_misc <- as.numeric(n_cur_misc[2])
    tbl_cur_result_misc <- tibble::tibble(Trait = tbl_search_misc$Trait[idx], MeanValue = n_cur_misc)
    if (is.null(tbl_result_misc)){
      tbl_result_misc <- tbl_cur_result_misc
    } else {
      tbl_result_misc <- dplyr::bind_rows(tbl_result_misc, tbl_cur_result_misc)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_prop, tbl_result_misc)


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for birth and weaning weight
  vec_ecow_result_birth_wean <- extract_result(ps_path_2outputfile,
                                               ps_start_statement2extract = tbl_output_statement[10,],
                                               ps_end_statement2extract = tbl_output_statement[11,],
                                               pb_log,
                                               plogger = lgr)
  tbl_result_birth_wean <- NULL
  tbl_search_birth_wean <- tbl_search[29:32,]
  for (idx in 1:nrow(tbl_search_birth_wean)){
    n_cur_birth_wean <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_birth_wean,
                                   ps_statement2search = tbl_search_birth_wean$SearchPattern[idx],
                                   ps_line2get = tbl_search_birth_wean$IndexOffset[idx],
                                   ps_splitby = ":",
                                   pb_log,
                                   plogger = lgr)
    n_cur_birth_wean <- as.numeric(n_cur_birth_wean[2])
    tbl_cur_result_birth_wean <- tibble::tibble(Trait = tbl_search_birth_wean$Trait[idx], MeanValue = n_cur_birth_wean)
    if (is.null(tbl_result_birth_wean)){
      tbl_result_birth_wean <- tbl_cur_result_birth_wean
    } else {
      tbl_result_birth_wean <- dplyr::bind_rows(tbl_result_birth_wean, tbl_cur_result_birth_wean)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_birth_wean)


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for carcass weight
  vec_ecow_result_slaughter <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = tbl_output_statement[12,],
                                              ps_end_statement2extract = tbl_output_statement[13,],
                                              pb_log,
                                              plogger = lgr)
  tbl_result_slaughter <- NULL
  tbl_search_slaughter <- tbl_search[33:34,]
  for (idx in 1:nrow(tbl_search_slaughter)){
    n_cur_slaughter <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_slaughter,
                                         ps_statement2search = tbl_search_slaughter$SearchPattern[idx],
                                         ps_line2get = tbl_search_slaughter$IndexOffset[idx],
                                         ps_splitby = ":",
                                         pb_log,
                                         plogger = lgr)
    n_cur_slaughter <- as.numeric(n_cur_slaughter[2])
    tbl_cur_result_slaughter <- tibble::tibble(Trait = tbl_search_slaughter$Trait[idx], MeanValue = n_cur_slaughter)
    if (is.null(tbl_result_slaughter)){
      tbl_result_slaughter <- tbl_cur_result_slaughter
    } else {
      tbl_result_slaughter <- dplyr::bind_rows(tbl_result_slaughter, tbl_cur_result_slaughter)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_slaughter)


  ### # Calculation of some average values
  Fleshiness <- ((tbl_result_mean$MeanValue[6]*tbl_result_mean$MeanValue[1]) + (tbl_result_mean$MeanValue[8]*tbl_result_mean$MeanValue[2]))/(tbl_result_mean$MeanValue[1]+tbl_result_mean$MeanValue[2])
  Fat <- ((tbl_result_mean$MeanValue[3]*tbl_result_mean$MeanValue[1]) + (tbl_result_mean$MeanValue[5]*tbl_result_mean$MeanValue[2]))/(tbl_result_mean$MeanValue[1]+tbl_result_mean$MeanValue[2])
  Birth_weight <- (tbl_result_mean$MeanValue[10]+tbl_result_mean$MeanValue[11])/2
  Weaning_weight_direct <- (tbl_result_mean$MeanValue[12]+tbl_result_mean$MeanValue[13])/2
  Weaning_weight_maternal <- Weaning_weight_direct

  l_constant <- get_constants()
  AgeAdjusted_Carcass_weight <- (tbl_result_mean$MeanValue[14]*l_constant$vec_dressing_male*tbl_result_mean$MeanValue[1] + tbl_result_mean$MeanValue[15]*l_constant$vec_dressing_female*tbl_result_mean$MeanValue[2])/(tbl_result_mean$MeanValue[1]+tbl_result_mean$MeanValue[2])


  ### # Tranformation for some values economic weight
  # For age corrected carcass weight we need to convert the economic values of average daily gain:
  # we divide the economic weight for ADG by the total number of days of fattening.
  # This gives the EV per gram increase in carcass weight → multiply by 1000 to get EV per kg increase in carcass weight.
  Slaughter_weight_EV <- tbl_result_ew$EconomicValue[6]/avg_fattening_length
  AACW_EW <- round(Slaughter_weight_EV*l_constant$vec_dressing_male*1000, digits = 2) #*0.58 to get from slaughter to carcass weight, *1000 to go from g to kg


  # ****************************************************************************
  ## ---- Combination of Results ----
  # ****************************************************************************
  tbl_aggregate_results <- tibble::tibble(Traits =  c("Calving_performance",
                                                      "Birth_weight",
                                                      "Age_adjusted_carcass_weight",
                                                      "Mean_class_fleshiness",
                                                      "Mean_class_fat",
                                                      "Weaning_weight_direct",
                                                      "Weaning_weight_maternal"),
                                          Economic_weight = c(tbl_result_ew$EconomicValue[1],
                                                              tbl_result_ew$EconomicValue[5],
                                                              AACW_EW,
                                                              tbl_result_ew$EconomicValue[10],
                                                              tbl_result_ew$EconomicValue[11],
                                                              tbl_result_ew$EconomicValue[16],
                                                              tbl_result_ew$EconomicValue[17]),
                                          Population_mean = c(tbl_result_mean$MeanValue[9],
                                                              Birth_weight,
                                                              AgeAdjusted_Carcass_weight,
                                                              Fleshiness,
                                                              Fat,
                                                              Weaning_weight_direct,
                                                              Weaning_weight_maternal))

  return(tbl_aggregate_results)


}

