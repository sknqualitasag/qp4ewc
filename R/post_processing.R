### #
### #
### #
### #   Purpose:   Function related to the post-processing steps
### #   started:   2022-05-20 (skn)
### #
### # ##################################################################### ###


#' @title Extract average fattening length from the output-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the average fattening length.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return avg_fattening_length output average fattening length
#'
#' @export extract_avg_fattening_length
extract_avg_fattening_length <- function(ps_path_2outputfile,
                                         ps_tbl_output_statement,
                                         ps_tbl_output_search_pattern,
                                         ps_prodsystem,
                                         pb_log,
                                         plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_avg_fattening_length.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_avg_fattening_length',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement \n',
                           ' * ps_tbl_output_search_pattern \n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n'))
  }
  
  
  ### # Setting the constants
  l_constants <- get_constants()
  # beef-on-beef constants
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    l_constants_postprocess <- get_constants_postprocess_beefOnbeef()
    l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  }else{
    # beef-on-dairy constants
    l_constants_postprocess <- get_constants_postprocess_beefOndairy()
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for fattening length (time from the end of rearing to slaughter) 
  # beef-on-beef variable
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    idx_row_fattened_bull <- l_constants_postprocess$idx_row_fattened_bulls
    idx_row_fattened_heiferm <- l_constants_postprocess$idx_row_fattened_heifers
    search_fatm <- l_constants_postprocess$idx_row_fat_length_m
  }else{
    # beef-on-dairy variable
    idx_row_fattened_bull <- l_constants_postprocess$idx_row_fattened_bull
    idx_row_fattened_heiferm <- l_constants_postprocess$idx_row_fattened_dairyheifer
    search_fatm <- l_constants_postprocess$search_fatm
  }
  
  
  ### # Fattening length for bulls
  vec_ecow_result_fat_male <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                             ps_start_statement2extract = ps_tbl_output_statement[idx_row_fattened_bull,],
                                             ps_end_statement2extract = ps_tbl_output_statement[idx_row_fattened_heiferm,],
                                             pb_log = pb_log,
                                             plogger = lgr)
  tbl_result_ew_fat_m <- NULL
  tbl_search_fat_m <- ps_tbl_output_search_pattern[search_fatm,]
  n_cur_ew_fat_m <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat_male,
                                     ps_statement2search = tbl_search_fat_m$SearchPattern,
                                     ps_line2get = tbl_search_fat_m$IndexOffset,
                                     ps_splitby = "    ",
                                     pb_log = pb_log,
                                     plogger = lgr)
  # beef-on-beef variable
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    n_cur_ew_fat_m <- n_cur_ew_fat_m[n_cur_ew_fat_m != ""]
    length_fattening_m <- as.numeric(n_cur_ew_fat_m[l_constants_postprocess$string_3])
  }else{
    # beef-on-dairy variable
    length_fattening_m <- as.numeric(n_cur_ew_fat_m[l_constants_postprocess$string_4])
  }
  
  
  ### # Fattening length for heifers
  # beef-on-beef variable
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    idx_row_fattened_heifers <- l_constants_postprocess$idx_row_fattened_heifers
    search_fatf <- l_constants_postprocess$idx_row_fat_length_f
    
    if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)){
      ps_end_statement2extract_fat_f <- ps_tbl_output_statement[l_constants_postprocess$idx_row_fattened_heifers1,]
    }else if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst3)){
      ps_end_statement2extract_fat_f <- ps_tbl_output_statement[l_constants_postprocess$idx_row_fattened_heifers3,]
    }
    
  }else{
    # beef-on-dairy variable
    idx_row_fattened_heifers <- l_constants_postprocess$idx_row_fattened_beefheifer
    ps_end_statement2extract_fat_f <- ps_tbl_output_statement[l_constants_postprocess$idx_row_heifers,]
    search_fatf <- l_constants_postprocess$search_fatf
    
  }
  vec_ecow_result_fat_heifer <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                               ps_start_statement2extract = ps_tbl_output_statement[idx_row_fattened_heifers,],
                                               ps_end_statement2extract = ps_end_statement2extract_fat_f,
                                               pb_log = pb_log,
                                               plogger = lgr)
  tbl_result_ew_fat_f <- NULL
  tbl_search_fat_f <- ps_tbl_output_search_pattern[search_fatf,]
  n_cur_ew_fat_f <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat_heifer,
                                     ps_statement2search = tbl_search_fat_f$SearchPattern,
                                     ps_line2get = tbl_search_fat_f$IndexOffset,
                                     ps_splitby = "    ",
                                     pb_log = pb_log,
                                     plogger = lgr)
  # beef-on-beef variable
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    n_cur_ew_fat_f <- n_cur_ew_fat_f[n_cur_ew_fat_f != ""]
    length_fattening_f <- as.numeric(n_cur_ew_fat_f[l_constants_postprocess$string_3])
  }else{
    # beef-on-dairy variable
    length_fattening_f <- as.numeric(n_cur_ew_fat_f[l_constants_postprocess$string_4])
  }
  
  
  
  ### # Calculate average fattening length between female and male
  avg_fattening_length <- (length_fattening_f + length_fattening_m)/2
  
  
  return(avg_fattening_length)
  
}


#' @title Extract calving score, fleshiness and fat from the output-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the calving score, fleshiness, fat of the population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess constants of postprocessing
#' @param ps_marketchannel market channel
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_misc tibble results with calving, fleshiness, fat
#'
#' @export extract_calv_flesh_fat_popmean
extract_calv_flesh_fat_popmean <- function(ps_path_2outputfile,
                                           ps_tbl_output_statement,
                                           ps_tbl_output_search_pattern,
                                           pl_constants_postprocess,
                                           ps_marketchannel,
                                           ps_prodsystem,
                                           pb_log,
                                           plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_calv_flesh_fat_popmean.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_calv_flesh_fat_popmean',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n'))
  }
  
  
  ### # Extract the population mean of the results coming from ECOWEIGHT output 
  l_constants <- get_constants()
  l_constants_progeny_beefOndairy<- get_constants_progeny_beefOndairy()
  # beef-on-beef variables
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    idx_row_miscellaneous <- pl_constants_postprocess$idx_row_misc
    idx_row_nutrition <- pl_constants_postprocess$idx_row_nutrition
  }else{
    # beef-on-dairy variables
    idx_row_miscellaneous <- pl_constants_postprocess$idx_row_miscellaneous
    idx_row_nutrition <- pl_constants_postprocess$idx_row_nutrition
  }
  ### # For ConventionalVeal, ConvenionalBeef, Natura-Beef :calving performance, fleshiness, fat
  ### # Whereas for Export only calving score and birth weight
  vec_ecow_result_misc <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                         ps_start_statement2extract = ps_tbl_output_statement[idx_row_miscellaneous,],
                                         ps_end_statement2extract = ps_tbl_output_statement[idx_row_nutrition,],
                                         pb_log = pb_log,
                                         plogger = lgr)
  tbl_result_misc <- NULL
  
  # beef-on-beef variables
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    tbl_search_misc <- ps_tbl_output_search_pattern[pl_constants_postprocess$search_misc,]
    s_splitby <- ":"
    string_misc <- pl_constants_postprocess$string_2
  }else{
    # beef-on-dairy variables
    s_splitby <- "     "
    string_misc <- pl_constants_postprocess$string_3
    
    # beef-on-dairy Export
    if(ps_marketchannel == l_constants_progeny_beefOndairy$export_calf){
      tbl_search_misc <- ps_tbl_output_search_pattern[pl_constants_postprocess$search_misc_export,]
    }else if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_beef || ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_calf){
      # beef-on-dairy ConventionalBeef or ConventionalVeal
      tbl_search_misc <- ps_tbl_output_search_pattern[pl_constants_postprocess$search_misc,]
    }
    
  }
  for (idx in 1:nrow(tbl_search_misc)){
    n_cur_misc <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_misc,
                                   ps_statement2search = tbl_search_misc$SearchPattern[idx],
                                   ps_line2get = tbl_search_misc$IndexOffset[idx],
                                   ps_splitby = s_splitby,
                                   pb_log = pb_log,
                                   plogger = lgr)
    n_cur_misc <- as.numeric(n_cur_misc[string_misc])
    tbl_cur_result_misc <- tibble::tibble(Trait = tbl_search_misc$Trait[idx], MeanValue = n_cur_misc)
    if (is.null(tbl_result_misc)){
      tbl_result_misc <- tbl_cur_result_misc
    } else {
      tbl_result_misc <- dplyr::bind_rows(tbl_result_misc, tbl_cur_result_misc)
    }
  }
  
  
  return(tbl_result_misc)
  
}


#' @title Extract slaughter weight from the output-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract slaughter weight population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess constants of postprocessing
#' @param ps_marketchannel market channel
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_slaughter tibble results with slaughter weight population mean
#'
#' @export extract_slaughterweight_popmean
extract_slaughterweight_popmean <- function(ps_path_2outputfile,
                                            ps_tbl_output_statement,
                                            ps_tbl_output_search_pattern,
                                            pl_constants_postprocess,
                                            ps_marketchannel,
                                            ps_prodsystem,
                                            pb_log,
                                            plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_slaughterweight_popmean.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_slaughterweight_popmean',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n'))
  }
  
  
  ### # Set the constants
  l_constants <- get_constants()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  # beef-on-beef variables
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    s_start_statement2extract <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_input08,]
    s_splitby <- ":"
    
    if(ps_prodsystem == l_constants_ewbc_input_beefOnbeef$prodsyst1){
      s_end_statement2extract_carcass <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_input05,]
    }else{
      s_end_statement2extract_carcass <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_input04,]
    }
    
  }else{
  # beef-on-dairy variables
    s_start_statement2extract <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_INPUT23,]
    s_end_statement2extract_carcass <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_INPUT22,]
    s_splitby <- "    "
  }
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for slaughter weight
  vec_ecow_result_slaughter <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = s_start_statement2extract,
                                              ps_end_statement2extract = s_end_statement2extract_carcass,
                                              pb_log = pb_log,
                                              plogger = lgr)
  tbl_result_slaughter <- NULL
  tbl_search_slaughter <- ps_tbl_output_search_pattern[pl_constants_postprocess$search_slaughter,]
  for (idx in 1:nrow(tbl_search_slaughter)){
    n_cur_slaughter <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_slaughter,
                                        ps_statement2search = tbl_search_slaughter$SearchPattern[idx],
                                        ps_line2get = tbl_search_slaughter$IndexOffset[idx],
                                        ps_splitby = s_splitby,
                                        pb_log = pb_log,
                                        plogger = lgr)
    n_cur_slaughter <- as.numeric(n_cur_slaughter[pl_constants_postprocess$string_2])
    tbl_cur_result_slaughter <- tibble::tibble(Trait = tbl_search_slaughter$Trait[idx], MeanValue = n_cur_slaughter)
    if (is.null(tbl_result_slaughter)){
      tbl_result_slaughter <- tbl_cur_result_slaughter
    } else {
      tbl_result_slaughter <- dplyr::bind_rows(tbl_result_slaughter, tbl_cur_result_slaughter)
    }
  }
  
  
  return(tbl_result_slaughter)
  
}


#' @title Extract birth or/and weaning weight from the output-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the birth or/and weaning weight population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess constants of postprocessing
#' @param ps_marketchannel market channel
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_birth_wean_wt tibble results with birth weight population mean
#'
#' @export extract_birth_weaning_weight_popmean
extract_birth_weaning_weight_popmean <- function(ps_path_2outputfile,
                                                 ps_tbl_output_statement,
                                                 ps_tbl_output_search_pattern,
                                                 pl_constants_postprocess,
                                                 ps_marketchannel,
                                                 ps_prodsystem,
                                                 pb_log,
                                                 plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_birth_weaning_weight_popmean.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_birth_weaning_weight_popmean',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem,'\n'))
  }
  
  
  ### # Set the constants
  l_constants <- get_constants()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  # beef-on-beef variables
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    s_start_statement2extract <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_input06,]
    s_splitby <- ":"
    search_bww <- pl_constants_postprocess$search_birth_wean
    string_bww <- pl_constants_postprocess$string_2
    
    if(ps_prodsystem == l_constants_ewbc_input_beefOnbeef$prodsyst1){
      s_end_statement2extract_carcass <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_input13,]
    }else{
      s_end_statement2extract_carcass <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_input08,]
    }
    
  }else{
    # beef-on-dairy variables
    s_start_statement2extract <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_INPUT15,]
    s_end_statement2extract_carcass <- ps_tbl_output_statement[pl_constants_postprocess$idx_row_INPUT11,]
    s_splitby <- "    "
    search_bww <- pl_constants_postprocess$search_bw
    string_bww <- pl_constants_postprocess$string_3
  }
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for birth weight
  vec_ecow_result_birth_wean_wt <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                            ps_start_statement2extract = s_start_statement2extract,
                                            ps_end_statement2extract = s_end_statement2extract_carcass,
                                            pb_log = pb_log,
                                            plogger = lgr)
  tbl_result_birth_wean <- NULL
  tbl_search_birthwt <- ps_tbl_output_search_pattern[search_bww,]
  for (idx in 1:nrow(tbl_search_birthwt)){
    n_cur_birth_wean <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_birth_wean_wt,
                                      ps_statement2search = tbl_search_birthwt$SearchPattern[idx],
                                      ps_line2get = tbl_search_birthwt$IndexOffset[idx],
                                      ps_splitby = s_splitby,
                                      pb_log = pb_log,
                                      plogger = lgr)
    
    n_cur_birth_wean <- as.numeric(n_cur_birth_wean[string_bww])
    tbl_cur_result_birth_wean <- tibble::tibble(Trait = tbl_search_birthwt$Trait[idx], MeanValue = n_cur_birth_wean)
    if (is.null(tbl_result_birth_wean)){
      tbl_result_birth_wean <- tbl_cur_result_birth_wean
    } else {
      tbl_result_birth_wean <- dplyr::bind_rows(tbl_result_birth_wean, tbl_cur_result_birth_wean)
    }
   }

  
  return(tbl_result_birth_wean)
  
}


#' @title Combination of the results from the output-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function combine economic weight and population mean.
#'
#' @param ptbl_popmean_results tibble with population mean out of ECOWEIGHT
#' @param ptbl_EW_results tibble with economic weight out of ECOWEIGHT
#' @param ps_scenario string for the scenario
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_aggregate_results result tibble with economic weight and population mean
#'
#' @export combine_popmean
combine_popmean <- function(ptbl_popmean_results,
                            ptbl_EW_results,
                            ps_scenario,
                            ps_prodsystem,
                            pb_log,
                            plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'combine_popmean.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'combine_popmean',
                    paste0('Starting function with parameters:\n * ptbl_popmean_results \n',
                           ' * ptbl_EW_results \n',
                           ' * ps_scenario: ', ps_scenario, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem,'\n'))
  }
  
  
  tbl_aggregate_results <- ptbl_EW_results %>% dplyr::inner_join(ptbl_popmean_results, by = "Traits")
  
  # Modify the format of the results
  tbl_aggregate_results$"EW (Population_mean)" <- paste0(tbl_aggregate_results$EW, "   (",tbl_aggregate_results$Population_mean,")")
  tbl_aggregate_results <- dplyr::select(tbl_aggregate_results, Traits, EW_unit, "EW (Population_mean)")
  tbl_aggregate_results <- data.frame(t(tbl_aggregate_results))
  tbl_aggregate_results <- tbl_aggregate_results[-1,]
  
  
  l_constants <- get_constants()
  # beef-on-beef variables
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    traits <- c("Calving_performance_direct_transformed",
                "Calving_performance_maternal_transformed",
                "Birth_weight_direct",
                "Birth_weight_maternal",
                "Age_adjusted_carcass_weight",
                "Mean_class_fleshiness",
                "Mean_class_fat",
                "Weaning_weight_direct",
                "Weaning_weight_maternal")
  }else{
  # beef-on-dairy variables
    traits <-  c("Calving_performance",
                 "Calving_performance_transformed",
                 "Birth_weight",
                 "Gestation_length",
                 "Age_adjusted_carcass_weight",
                 "Mean_class_fleshiness",
                 "Mean_class_fat")
  }
  
  
  colnames(tbl_aggregate_results) <- traits
  rownames(tbl_aggregate_results) <- c("EW_unit", paste0(ps_scenario))
  
  return(tbl_aggregate_results)
  
}


#' @title Post-processing infos from the output-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function process this output file.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_outputfilenameECOWEIGHT name output file of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_path_tbl_save path to save results
#' @param ps_scenario name of the scenario (includes sire breed, dam breed, production system and marketing channel)
#' @param ps_input_genetic_SD input file with genetic standarddeviation
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @export post_process_output
post_process_output <- function(ps_path_2outputfile,
                                ps_outputfilenameECOWEIGHT,
                                ps_output_statement,
                                ps_output_search_pattern,
                                ps_sirebreed,
                                ps_dambreed,
                                ps_prodsystem,
                                ps_marketchannel,
                                ps_path_directory2create,
                                ps_scenario,
                                ps_path_tbl_save,
                                ps_input_genetic_SD,
                                pb_log,
                                plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'post_process_output.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'post_process_output',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_outputfilenameECOWEIGHT: ',ps_outputfilenameECOWEIGHT,'\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel,'\n',
                           ' * ps_path_directory2create: ',ps_path_directory2create,'\n',
                           ' * ps_scenario: ',ps_scenario,'\n',
                           ' * ps_path_tbl_save: ',ps_path_tbl_save,'\n',
                           ' * ps_input_genetic_SD: ',ps_input_genetic_SD,'\n'))
  }
  
  
  l_constants <- get_constants()
  # beef-on-beef variables
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    ### # Extract economic weights from the output-parameter-file of ECOWEIGHT beef on beef
    tbl_result_ew <- extract_ewbc(ps_path_2outputfile = file.path(ps_path_2outputfile,ps_scenario,ps_outputfilenameECOWEIGHT),
                                  ps_output_statement = ps_output_statement,
                                  ps_output_search_pattern = ps_output_search_pattern,
                                  ps_sirebreed = ps_sirebreed,
                                  ps_dambreed = ps_dambreed,
                                  ps_prodsystem = ps_prodsystem,
                                  ps_marketchannel = ps_marketchannel,
                                  ps_path_directory2create = ps_path_directory2create,
                                  pb_log = pb_log,
                                  plogger = lgr)
    
    ### # Extract or calculate population mean from the output-parameter-file of ECOWEIGHT beef on beef
    tbl_result_popmean <- extract_popmean_ewbc(ps_path_2outputfile = file.path(ps_path_2outputfile,ps_scenario,ps_outputfilenameECOWEIGHT),
                                               ps_output_statement = ps_output_statement,
                                               ps_output_search_pattern = ps_output_search_pattern,
                                               ps_sirebreed = ps_sirebreed,
                                               ps_dambreed = ps_dambreed,
                                               ps_prodsystem = ps_prodsystem,
                                               ps_marketchannel = ps_marketchannel,
                                               ps_path_directory2create = ps_path_directory2create,
                                               pb_log = pb_log,
                                               plogger = lgr)
    
  }else{
    # beef-on-dairy variables
    ### # Extract or calculate population mean from the output-parameter-file of ECOWEIGHT beef on dairy
    tbl_result_ew <- extract_ewdc(ps_path_2outputfile = file.path(ps_path_2outputfile,ps_scenario,ps_outputfilenameECOWEIGHT),
                                  ps_path_2outputfile_run1 = file.path(ps_path_2outputfile,paste0(ps_scenario,"_glPlus1",collapse = ""),ps_outputfilenameECOWEIGHT),
                                  ps_output_statement = ps_output_statement,
                                  ps_output_search_pattern = ps_output_search_pattern,
                                  ps_sirebreed = ps_sirebreed,
                                  ps_dambreed = ps_dambreed,
                                  ps_prodsystem = ps_prodsystem,
                                  ps_marketchannel = ps_marketchannel,
                                  ps_path_directory2create = ps_path_directory2create,
                                  pb_log = pb_log,
                                  plogger = lgr)
    
    ### # Extract or calculate population mean from the output-parameter-file of ECOWEIGHT beef on dairy
    tbl_result_popmean <- extract_popmean_ewdc(ps_path_2outputfile = file.path(ps_path_2outputfile,ps_scenario,ps_outputfilenameECOWEIGHT),
                                               ps_output_statement = ps_output_statement,
                                               ps_output_search_pattern = ps_output_search_pattern,
                                               ps_sirebreed = ps_sirebreed,
                                               ps_dambreed = ps_dambreed,
                                               ps_prodsystem = ps_prodsystem,
                                               ps_marketchannel = ps_marketchannel,
                                               ps_path_directory2create = ps_path_directory2create,
                                               pb_log = pb_log,
                                               plogger = lgr)
    
  }
  
  
  ### # Combination of the results from the output-parameter-file of ECOWEIGHT beef on dairy
  tbl_aggregate_results <- combine_popmean(ptbl_popmean_results = tbl_result_popmean,
                                           ptbl_EW_results = tbl_result_ew,
                                           ps_scenario = ps_scenario,
                                           ps_prodsystem = ps_prodsystem,
                                           pb_log = pb_log,
                                           plogger = lgr)
  
  ### # Save table and piechart in pdf
  save_csv_table_piechart(ps_tbl_aggregate_results = tbl_aggregate_results,
                          ptbl_EW_results = tbl_result_ew,
                          ps_path_tbl_save = ps_path_tbl_save,
                          ps_scenario = ps_scenario,
                          ps_marketchannel = ps_marketchannel,
                          ps_prodsystem = ps_prodsystem,
                          ps_input_genetic_SD = ps_input_genetic_SD,
                          pb_log = pb_log,
                          plogger = lgr)
  
  
}  


#' @title Save table and piechart in pdf
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function process the results in a pdf.
#'
#' @param ps_tbl_aggregate_results tibble with economic weight and population mean
#' @param ptbl_EW_results tibble of economic weights
#' @param ps_path_tbl_save path to save results
#' @param ps_scenario name of the scenario (includes sire breed, dam breed, production system and marketing channel)
#' @param ps_marketchannel marketing channel required for determining the correct genetic standard deviation to use
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_input_genetic_SD input file with genetic standarddeviation
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @import ggplot2
#' @import forcats
#'
#' @export save_csv_table_piechart
save_csv_table_piechart <- function(ps_tbl_aggregate_results,
                                    ptbl_EW_results,
                                    ps_path_tbl_save,
                                    ps_scenario,
                                    ps_marketchannel,
                                    ps_prodsystem,
                                    ps_input_genetic_SD,
                                    pb_log,
                                    plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'save_csv_table_piechart.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'save_csv_table_piechart',
                    paste0('Starting function with parameters:\n * ps_tbl_aggregate_results \n',
                           ' * ptbl_EW_results \n',
                           ' * ps_path_tbl_save: ', ps_path_tbl_save, '\n',
                           ' * ps_scenario: ',ps_scenario, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem,'\n',
                           ' * ps_input_genetic_SD: ',ps_input_genetic_SD, '\n'))
  }
  
  
  l_constants_progeny_beefOndairy<- get_constants_progeny_beefOndairy()
  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()
  l_constants <- get_constants()
  
  
  ### # write csv-file with result                                         
  if(!dir.exists(ps_path_tbl_save)) dir.create(ps_path_tbl_save, recursive = TRUE)
  write.csv(ps_tbl_aggregate_results, file = file.path(ps_path_tbl_save, paste0("df_", ps_scenario, ".csv", collapse = "")), row.names = TRUE)
  
  ### # Build piechart
  # beef-on-beef
  if(ps_prodsystem != as.character(l_constants$prodsyst4)){
    pie_chart_functional <-  plot_piechart_ewbc(ps_path_2genSD = ps_input_genetic_SD,
                                                ptbl_EW_results = ptbl_EW_results,
                                                ps_traitgroup2consider = "Functional Traits",
                                                ps_scenario = ps_scenario,
                                                ps_prodsystem = ps_prodsystem,
                                                pb_log = pb_log,
                                                plogger = lgr)
    
    pie_chart_carcass <-  plot_piechart_ewbc(ps_path_2genSD = ps_input_genetic_SD,
                                             ptbl_EW_results = ptbl_EW_results,
                                             ps_traitgroup2consider = "Carcass Traits",
                                             ps_scenario = ps_scenario,
                                             ps_prodsystem = ps_prodsystem,
                                             pb_log = pb_log,
                                             plogger = lgr)
    
    pie_chart_combined <-  plot_piechart_ewbc(ps_path_2genSD = ps_input_genetic_SD,
                                              ptbl_EW_results = ptbl_EW_results,
                                              ps_traitgroup2consider = "Combined",
                                              ps_scenario = ps_scenario,
                                              ps_prodsystem = ps_prodsystem,
                                              pb_log = pb_log,
                                              plogger = lgr)

  }else{
    # beef-on-dairy
    pie_chart_functional <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                                ptbl_EW_results = ptbl_EW_results,
                                                ps_traitgroup2consider = "Functional Traits",
                                                ps_scenario = ps_scenario,
                                                ps_marketchannel = ps_marketchannel,
                                                pb_log = pb_log,
                                                plogger = lgr)

    # ConventionalBeef or ConventionalVeal
    if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
      pie_chart_carcass <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                               ptbl_EW_results = ptbl_EW_results,
                                               ps_traitgroup2consider = "Carcass Traits",
                                               ps_scenario = ps_scenario,
                                               ps_marketchannel = ps_marketchannel,
                                               pb_log = pb_log,
                                               plogger = lgr)
      
      pie_chart_combined <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                                ptbl_EW_results = ptbl_EW_results,
                                                ps_traitgroup2consider = "Combined",
                                                ps_scenario = ps_scenario,
                                                ps_marketchannel = ps_marketchannel,
                                                pb_log = pb_log,
                                                plogger = lgr)
    }
    
    
  }
  
  
  ### # save table and pie charts to pdf
  opar <- par()
  pdf(file = file.path(ps_path_tbl_save, paste0("plots_", ps_scenario, ".pdf", collapse = "")), onefile = TRUE, width = 25)
  gridExtra::grid.table(ps_tbl_aggregate_results)
  par(mfrow = c(3,1))
  if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
    print(pie_chart_functional)
    print(pie_chart_carcass)
    print(pie_chart_combined)
  }else{
    print(pie_chart_functional)
  }
  par(opar)
  dev.off()
  
}


#' @title Extract economic weights from the output-parameter-file of ECOWEIGHT beef on beef
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the economic weights.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_aggregate_results result tibble with economic weights and units
#'
#' @export extract_ewbc
extract_ewbc <- function(ps_path_2outputfile,
                         ps_output_statement,
                         ps_output_search_pattern,
                         ps_sirebreed,
                         ps_dambreed,
                         ps_prodsystem,
                         ps_marketchannel,
                         ps_path_directory2create,
                         pb_log,
                         plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_ewbc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_ewbc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel,'\n',
                           ' * ps_path_directory2create: ',ps_path_directory2create,'\n'))
  }
  
  
  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- read_file_input(ps_input_file = ps_output_statement,
                                          pb_log = pb_log,
                                          plogger = lgr)
  
  
  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log = pb_log,
                                plogger = lgr)
  
  
  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()
  l_constants <- get_constants()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output (section 3.12)
  ### # explanations under https://qualitasag.atlassian.net/wiki/spaces/ZWS/pages/2965569565/20220728+--+Weekly+Meeting+Projekt+Gesamtzuchtwert+mit+Produktionsmodellen
  vec_ecow_result_EW <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                       ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_EW_d_m,],
                                       ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_EW_relative,],
                                       pb_log = pb_log,
                                       plogger = lgr)
  
  
  ### # Get the value
  tbl_result_ew <- NULL
  for(idx in 1:nrow(tbl_search[l_constants_postprocess_beefOnbeef$search_ew,])){
    n_cur_ew <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_EW,
                                 ps_statement2search = tbl_search$SearchPattern[idx],
                                 ps_line2get = tbl_search$IndexOffset[idx],
                                 ps_splitby = "  ",
                                 pb_log = pb_log,
                                 plogger = lgr)
    n_cur_ew <- n_cur_ew[n_cur_ew != ""]
    n_cur_ew_direct <- as.numeric(n_cur_ew[l_constants_postprocess_beefOnbeef$string_1])
    n_cur_ew_maternal <- as.numeric(n_cur_ew[l_constants_postprocess_beefOnbeef$string_2])
    tbl_cur_ew <- tibble::tibble(Trait = tbl_search$Trait[idx], EconomicValueDirect = n_cur_ew_direct, EconomicValueMaternal = n_cur_ew_maternal)
    
    if (is.null(tbl_result_ew)){
      tbl_result_ew <- tbl_cur_ew
    } else {
      tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_cur_ew)
    }
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for fattening length
  ### # Average fattening length is required for calculating the economic weight for age adjusted carcass weight: EW ADG/days fattening
  avg_length_fat <- extract_avg_fattening_length(ps_path_2outputfile = ps_path_2outputfile,
                                                 ps_tbl_output_statement = tbl_output_statement,
                                                 ps_tbl_output_search_pattern = tbl_search,
                                                 ps_prodsystem = ps_prodsystem,
                                                 pb_log = pb_log,
                                                 plogger = lgr)

  
  ### # Calculation of Economic Weight for age adjusted carcass weight 
  ### # We need to convert the economic values of average daily gain (ADG):
  ### # Divide the economic weight for ADG by the total number of days of fattening.
  ### # This gives the economic value (EV) per gram increase in carcass weight → multiply by 1000 to get EV per kg increase in carcass weight 
  ### # Conversion with dressing percentage to move from live weight at slaughter to carcass weight
  AASW_EW <- (tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_ADG])/(avg_length_fat)*1000
  ACCW_EW <- AASW_EW*l_constants$dressingpercentage_female
  tbl_ACCW <- tibble::tibble(Trait = "EWAgeCorrectedCarcassWeight", EconomicValueDirect = ACCW_EW, EconomicValueMaternal = NA)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_ACCW)
  
  
  ### # Read csv-file containing mean and standarddeviation of raw and transformed phenotype for calving score
  tbl_mean_sd <- readr::read_delim(file = file.path(ps_path_directory2create,
                                                    paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystem,"_",ps_marketchannel, collapse = ""),
                                                    "mean_sd_calvingscore.csv"), 
                                   delim = ",")
  
  
  # get the economic weight out of the result file from ECOWEIGHT (EWBC)
  EW_calving_direct <- tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_calving]
  EW_calving_maternal <- tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_calving]
  
  # economic weight per genetic standardeviation based on the raw phenotype scale
  ew_sd_direct <- as.numeric(EW_calving_direct)*tbl_mean_sd$sd_raw_calvscore/l_constants_postprocess_beefOnbeef$calving_t_delta
  ew_sd_maternal <- as.numeric(EW_calving_maternal)*tbl_mean_sd$sd_raw_calvscore/l_constants_postprocess_beefOnbeef$calving_t_delta

  # economic weight per genetic standardeviation based on the transformed phenotype scale
  ew_u_direct = -(ew_sd_direct/tbl_mean_sd$sd_transform_calvscore)
  ew_u_maternal = -(ew_sd_maternal/tbl_mean_sd$sd_transform_calvscore)

  #Add transformed EW for calving score to the EW table
  tbl_transformed_dir <- tibble::tibble(Trait = "EWCalvingPerformanceTransform", EconomicValueDirect = ew_u_direct, EconomicValueMaternal = ew_u_maternal)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_transformed_dir)
  
  
  # Build a tibble
  traits <- c("Calving_performance_direct_transformed",
              "Calving_performance_maternal_transformed",
              "Birth_weight_direct",
              "Birth_weight_maternal",
              "Age_adjusted_carcass_weight",
              "Mean_class_fleshiness",
              "Mean_class_fat",
              "Weaning_weight_direct",
              "Weaning_weight_maternal")
  
  ew_unit = c("CHF/0.01 transformed score",
              "CHF/0.01 transformed score",
              "CHF/kg",
              "CHF/kg",
              "CHF/kg",
              "CHF/0.01 score",
              "CHF/0.01 score",
              "CHF/kg",
              "CHF/kg")
  
  if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)){
     ew <- c(round(ew_u_direct, digits = 2),
             round(ew_u_maternal, digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2),
             round(tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_ACCW], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fleshiness], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fat], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2),
             round(tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2))
  }else if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst3)){
     ew <- c(round(ew_u_direct, digits = 2),
             "NA",
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2),
             "NA",
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_ACCW], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fleshiness], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fat], digits = 2),
             round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2),
             "NA")
  }
  
  tbl_aggregate_results <- tibble::tibble(Traits =  traits, EW = ew, EW_unit = ew_unit)
    
  return(tbl_aggregate_results)
  
}


#' @title Extract or calculate population mean from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_aggregate_results result tibble with population mean
#'
#' @export extract_popmean_ewbc
extract_popmean_ewbc <- function(ps_path_2outputfile,
                                 ps_output_statement,
                                 ps_output_search_pattern,
                                 ps_sirebreed,
                                 ps_dambreed,
                                 ps_prodsystem,
                                 ps_marketchannel,
                                 ps_path_directory2create,
                                 pb_log,
                                 plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_popmean_ewbc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_popmean_ewbc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel,'\n',
                           ' * ps_path_directory2create: ',ps_path_directory2create,'\n'))
  }
  
  
  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- read_file_input(ps_input_file = ps_output_statement,
                                          pb_log = pb_log,
                                          plogger = lgr)
  
  
  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log = pb_log,
                                plogger = lgr)
  
  
  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  l_constant <- get_constants()
  
  
  ### # Extract proportion male and female from the output-parameter-file of ECOWEIGHT beef on beef
  ### # How many females are kept for replacement will affect how to calculate the average trait values at slaughter
  tbl_result_prop <- extract_propmale_female_popmean_ewbc(ps_path_2outputfile = ps_path_2outputfile,
                                                          ps_tbl_output_statement = tbl_output_statement,
                                                          ps_tbl_output_search_pattern = tbl_search,
                                                          pl_constants_postprocess = l_constants_postprocess_beefOnbeef,
                                                          ps_marketchannel = ps_marketchannel,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
  tbl_result_mean <- tbl_result_prop
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for calving performance, fleshiness, fat
  tbl_result_misc <- extract_calv_flesh_fat_popmean(ps_path_2outputfile = ps_path_2outputfile,
                                                    ps_tbl_output_statement = tbl_output_statement,
                                                    ps_tbl_output_search_pattern = tbl_search,
                                                    pl_constants_postprocess = l_constants_postprocess_beefOnbeef,
                                                    ps_marketchannel = ps_marketchannel,
                                                    ps_prodsystem = ps_prodsystem,
                                                    pb_log = pb_log,
                                                    plogger = lgr)
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_misc)
  
  ### # Extract birth and weaning weight from the output-parameter-file of ECOWEIGHT beef on beef
  tbl_result_birth_wean <- extract_birth_weaning_weight_popmean(ps_path_2outputfile = ps_path_2outputfile,
                                                                ps_tbl_output_statement = tbl_output_statement,
                                                                ps_tbl_output_search_pattern = tbl_search,
                                                                pl_constants_postprocess = l_constants_postprocess_beefOnbeef,
                                                                ps_marketchannel = ps_marketchannel,
                                                                ps_prodsystem = ps_prodsystem,
                                                                pb_log = pb_log,
                                                                plogger = lgr)
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_birth_wean)
  
  ### # Extract slaughter weight from the output-parameter-file of ECOWEIGHT beef on beef
  tbl_result_slaughter <- extract_slaughterweight_popmean(ps_path_2outputfile = ps_path_2outputfile,
                                                          ps_tbl_output_statement = tbl_output_statement,
                                                          ps_tbl_output_search_pattern = tbl_search,
                                                          pl_constants_postprocess = l_constants_postprocess_beefOnbeef,
                                                          ps_marketchannel = ps_marketchannel,
                                                          ps_prodsystem = ps_prodsystem,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_slaughter)
  
  
  ### # Calculation of some average values
  if(ps_prodsystem == l_constants_ewbc_input_beefOnbeef$prodsyst1) {
    fleshiness_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_flesh_m_1
    prop_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_prop_m
    fleshiness_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_flesh_f_1
    prop_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_prop_f
    fat_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_fat_m
    fat_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_fat_f
    bw_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_bw_m_1
    bw_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_bw_f_1
    wean_wt_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_wean_m_1
    wean_wt_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_wean_f_1
    slaughter_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_slaughter_m_1
    slaughter_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_slaughter_f_1

  } else if(ps_prodsystem == l_constants_ewbc_input_beefOnbeef$prodsyst3){
    fleshiness_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_flesh_m
    prop_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_prop_m
    fleshiness_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_flesh_f
    prop_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_prop_f
    fat_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_fat_m
    fat_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_fat_h_1
    bw_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_bw_m
    bw_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_bw_f
    wean_wt_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_wean_m
    wean_wt_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_wean_f
    slaughter_m <- l_constants_postprocess_beefOnbeef$idx_row_avg_slaughter_m
    slaughter_f <- l_constants_postprocess_beefOnbeef$idx_row_avg_slaughter_f
  }
  
  
  Fleshiness <- ((tbl_result_mean$MeanValue[fleshiness_m]*tbl_result_mean$MeanValue[prop_m]) + (tbl_result_mean$MeanValue[fleshiness_f]*tbl_result_mean$MeanValue[prop_f]))/(tbl_result_mean$MeanValue[prop_m]+tbl_result_mean$MeanValue[prop_f])
  Fat <- ((tbl_result_mean$MeanValue[fat_m]*tbl_result_mean$MeanValue[prop_m]) + (tbl_result_mean$MeanValue[fat_f]*tbl_result_mean$MeanValue[prop_f]))/(tbl_result_mean$MeanValue[prop_m]+tbl_result_mean$MeanValue[prop_f])
  Birth_weight <- (tbl_result_mean$MeanValue[bw_m]+tbl_result_mean$MeanValue[bw_f])/2
  Weaning_weight <- (tbl_result_mean$MeanValue[wean_wt_m]+tbl_result_mean$MeanValue[wean_wt_f])/2
  AgeAdjusted_Carcass_weight <- (tbl_result_mean$MeanValue[slaughter_m]*l_constant$dressingpercentage_male*tbl_result_mean$MeanValue[prop_m] + tbl_result_mean$MeanValue[slaughter_f]*l_constant$dressingpercentage_female*tbl_result_mean$MeanValue[prop_f])/(tbl_result_mean$MeanValue[prop_m]+tbl_result_mean$MeanValue[prop_f])
  
  ### # Read csv-file containing mean and standarddeviation of raw and transformed phenotype for calving score
  tbl_mean_sd <- readr::read_delim(file = file.path(ps_path_directory2create,
                                                    paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystem,"_",ps_marketchannel, collapse = ""),
                                                    "mean_sd_calvingscore.csv"), 
                                   delim = ",")
  
  
  # Build a tibble
  traits <- c("Calving_performance_direct_transformed",
              "Calving_performance_maternal_transformed",
              "Birth_weight_direct",
              "Birth_weight_maternal",
              "Age_adjusted_carcass_weight",
              "Mean_class_fleshiness",
              "Mean_class_fat",
              "Weaning_weight_direct",
              "Weaning_weight_maternal")
  
  if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)){
     population_mean <- c(round(tbl_mean_sd$mean_transform_calvscore, digits = 2),
                          round(tbl_mean_sd$mean_transform_calvscore, digits = 2),
                          round(Birth_weight, digits = 2),
                          round(Birth_weight, digits = 2),
                          round(AgeAdjusted_Carcass_weight, digits = 2),
                          round(Fleshiness, digits = 2),
                          round(Fat, digits = 2),
                          round(Weaning_weight, digits = 2),
                          round(Weaning_weight, digits = 2))
  }else if (ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst3)){
      population_mean <- c(round(tbl_mean_sd$mean_transform_calvscore, digits = 2),
                           "NA",
                           round(Birth_weight, digits = 2),
                           "NA",
                           round(AgeAdjusted_Carcass_weight, digits = 2),
                           round(Fleshiness, digits = 2),
                           round(Fat, digits = 2),
                           round(Weaning_weight, digits = 2),
                           "NA")
  }
  
  tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                          Population_mean = population_mean)
                                                               
  
  return(tbl_aggregate_results)
  
  
}


#' @title Extract proportion male and female from the output-parameter-file of ECOWEIGHT beef on beef
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the proportion of male and female for the population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess_beefOnbeef constants of postprocessing beefOndbeef
#' @param ps_marketchannel market channel
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_prop tibble results with proportion of male and female
#'
#' @export extract_propmale_female_popmean_ewbc
extract_propmale_female_popmean_ewbc <- function(ps_path_2outputfile,
                                                 ps_tbl_output_statement,
                                                 ps_tbl_output_search_pattern,
                                                 pl_constants_postprocess_beefOnbeef,
                                                 ps_marketchannel,
                                                 pb_log,
                                                 plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_propmale_female_popmean_ewbc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_propmale_female_popmean_ewbc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement \n',
                           ' * ps_tbl_output_search_pattern \n',
                           ' * pl_constants_postprocess_beefOnbeef \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n'))
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for proportion of males and females
  ### # How many females are kept for replacement will affect how to calculate the average trait values at slaughter
  vec_ecow_result_prop <- extract_result(ps_path_2outputfile,
                                         ps_start_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOnbeef$idx_row_structure,],
                                         ps_end_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOnbeef$idx_row_growth,],
                                         pb_log,
                                         plogger = lgr)
  tbl_result_prop <- NULL
  tbl_search_prop <- ps_tbl_output_search_pattern[pl_constants_postprocess_beefOnbeef$idx_row_prop,]
  for (idx in 1:nrow(tbl_search_prop)){
    n_cur_prop <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_prop,
                                   ps_statement2search = tbl_search_prop$SearchPattern[idx],
                                   ps_line2get = tbl_search_prop$IndexOffset[idx],
                                   ps_splitby = ":",
                                   pb_log,
                                   plogger = lgr)
    n_cur_prop <- as.numeric(n_cur_prop[pl_constants_postprocess_beefOnbeef$string_2])
    tbl_cur_result_prop <- tibble::tibble(Trait = tbl_search_prop$Trait[idx], MeanValue = n_cur_prop)
    if (is.null(tbl_result_prop)){
      tbl_result_prop <- tbl_cur_result_prop
    } else {
      tbl_result_prop <- dplyr::bind_rows(tbl_result_prop, tbl_cur_result_prop)
    }
  }
  
  
  return(tbl_result_prop)
  
  
}


#' @title Create table of results depending on sire breed, dam breed or production system
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to prepare information to be plot.
#'
#' @param ps_sort_by parameter to determine how to sort the tables: sire_breed, dam_breed or production_system
#' @param ps_path_results_tbl path to results tables from initial post processing
#' @param ps_path_tbl_save path to save the results: cannot be the same path as the results tables from inital post processing
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @import ggplot2
#' @import forcats
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import gridExtra
#'
#' @export create_table_results_ewbc
create_table_results_ewbc <- function(ps_sort_by,
                                      ps_path_results_tbl,
                                      ps_path_save,
                                      pb_log,
                                      plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'create_table_results_ewbc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'create_table_results_ewbc',
                    paste0('Starting function with parameters:\n * ps_sort_by', ps_sort_by, '\n',
                           ' * ps_path_results_tbl: ', ps_path_results_tbl, '\n',
                           ' * ps_path_save: ', ps_path_save, '\n'))
  }
  

  ### # Set constant
  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()

  ### # List all .csv file in ps_path_results_tbl
  temp = list.files(path = ps_path_results_tbl, pattern="df_")
  results_tables <- data.frame(temp)

  ### # Look content all .csv file
  myfiles = lapply(paste0(ps_path_results_tbl, "/", temp), read.table, sep=",")

  ### # Extract information like sire, dam, marketing from the name of the files
  tbl_info <- NULL
  for(idx in 1:nrow(results_tables)){
    scenario_split <- unlist(strsplit(results_tables[idx,], split = "_", fixed = TRUE))
    scenario_split <- unlist(strsplit(scenario_split, split = ".", fixed = TRUE))
    sire <- scenario_split[l_constants_postprocess_beefOnbeef$string_2]
    dam <- scenario_split[l_constants_postprocess_beefOnbeef$string_3]
    production <- scenario_split[l_constants_postprocess_beefOnbeef$string_4]


    tbl_cur_info <- tibble::tibble(Sire = sire, Dam = dam, Production = production)
    if (is.null(tbl_info)){
      tbl_info <- tbl_cur_info
    } else {
      tbl_info <- dplyr::bind_rows(tbl_info, tbl_cur_info)
    }
  }

  
  ### # Find the criteria to sort
  if(ps_sort_by == "sire_breed") {
    criteria2sort <- unique(tbl_info$Sire)
    criteria <- "sire"
  }else if (ps_sort_by == "dam_breed") {
    criteria2sort <- unique(tbl_info$Dam)
    criteria <- "dam"
  } else if (ps_sort_by == "production_system") {
    criteria2sort <- unique(tbl_info$Production)
    criteria <- "production_system"
  }

  
  #To make sure the strings are correctly found in the scenario name rather than from elsewhere in the table
  for(idx in 1:length(criteria2sort)){
    if(ps_sort_by == "production_system"){
      breed <- paste0("_", criteria2sort[idx], "_")
    } else if (ps_sort_by == "sire_breed"){
      breed <- paste0(criteria2sort[idx], "_")
    } else {
      breed <- paste0("_", criteria2sort[idx])
    }


    tbl_sex_breed <- myfiles[grep(breed, myfiles)]
    tbl_breed <- NULL



    for (jdx in 1:length(tbl_sex_breed)){
      tbl_current_breed <- as.data.frame(tbl_sex_breed[jdx])
      colnames(tbl_current_breed) <- tbl_current_breed[1,]
      rownames(tbl_current_breed) <- tbl_current_breed[,1]
      tbl_current_breed <- tbl_current_breed[-1,]
      tbl_current_breed <- tbl_current_breed[,-1]

      if(is.null(tbl_breed)){
        tbl_breed <- tbl_current_breed
      } else {
        tbl_breed <- dplyr::bind_rows(tbl_breed, tbl_current_breed[2,])
      }
    }


    tbl_breed$Production_system <- NA
    tbl_breed$SirexDam <- NA

    for(n in 2:nrow(tbl_breed)){
      column_split <- unlist(strsplit(row.names(tbl_breed)[n], "_", fixed = TRUE))
      sire <- column_split[l_constants_postprocess_beefOnbeef$string_1]
      dam <- column_split[l_constants_postprocess_beefOnbeef$string_2]
      production_sys <- column_split[l_constants_postprocess_beefOnbeef$string_3]
      tbl_breed$SirexDam[n] <- paste0(sire, "_", dam)
      tbl_breed$Production_system[n] <- production_sys
    }


    tbl_breed <- tbl_breed %>%
      dplyr::relocate("SirexDam") %>%
      dplyr::relocate("Production_system")

    tbl_breed[is.na(tbl_breed)] <- "-"
    tbl_breed <- tbl_breed[order((tbl_breed$Production_system)), ]
    row.names(tbl_breed) <- NULL

    pdf(paste0(ps_path_save,"/results_tbl_",sex,"_",breed,".pdf"), height=11, width=30)
    gridExtra::grid.table(tbl_breed)
    dev.off()
  }

}


#' @title Plot pie chart of the results coming from ECOWEIGHT beef cattle
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to prepare information to be plot.
#'
#' @param ps_path_2genSD path to file with genetic standard deviation
#' @param ptbl_EW_results tibble of economic weights
#' @param ps_traitgroup2consider traitgroup may be Carcass or Functional Traits
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_scenario scenario name
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @import ggplot2
#' @import forcats
#'
#' @export plot_piechart_ewbc
plot_piechart_ewbc <- function(ps_path_2genSD,
                               ptbl_EW_results,
                               ps_traitgroup2consider,
                               ps_scenario,
                               ps_prodsystem,
                               pb_log,
                               plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'plot_piechart_ewbc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'plot_piechart_ewbc',
                    paste0('Starting function with parameters:\n * ps_path_2genSD', ps_path_2genSD,'\n',
                           ' * ptbl_aggregate_results \n',
                           ' * ps_traitgroup2consider: ', ps_traitgroup2consider, '\n',
                           ' * ps_scenario: ', ps_scenario, '\n'))
  }


  ### # Read file with genetic standard deviation
  tbl_gen_SD <- read_file_input(ps_input_file = ps_path_2genSD,
                                pb_log = pb_log,
                                plogger = lgr)

  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()

  if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)) {
    genetic_SD_calving_mat <- (tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_row_calving_maternal])
    genetic_SD_BW_mat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_birth_weight_maternal]
    genetic_SD_Wean_wt_mat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$weaning_maternal]
  }

  genetic_SD_calving_dir <- (tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_row_calving_direct])
  genetic_SD_BW_dir <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_birth_weight_direct]
  genetic_SD_Wean_wt_dir <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$weaning_direct]

  genetic_SD_ACCW <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_row_ACCW_Natura]
  gemetic_SD_fleshiness <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_row_fleshiness_Natura]
  genetic_SD_fat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOnbeef$idx_row_fat_Natura]

  
  
  ### # Take economic weights from table and transform them to same unit as EBV
  if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)){
    EW_calving_score_dir <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_calving_dir_transform])
    EW_calving_score_mat <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_calving_mat_transform])
    EW_birthwt_dir <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_birthwt_dir])
    EW_birthwt_mat <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_birthwt_mat])
    #EBV is in unit dt whereas EW is in unit kg
    EW_ACCW <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_ACCW])*100
    # unit 1 score
    EW_fleshiness <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_fleshiness])*100
    # unit 1 score
    EW_fat <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_fat])*100
    EW_Weantwt_dir <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_weanwt_dir])
    EW_Weantwt_mat <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_weanwt_mat])
  }else{
    EW_calving_score_dir <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_calving_dir_transform])
    EW_birthwt_dir <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_birthwt_dir])
    #EBV is in unit dt whereas EW is in unit kg
    EW_ACCW <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_ACCW])*100
    # unit 1 score
    EW_fleshiness <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_fleshiness])*100
    # unit 1 score
    EW_fat <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_fat])*100
    EW_Weantwt_dir <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOnbeef$ew_pie_weanwt_dir])
}
  

  ### # Ensure the economic weight is positive using absolute value for calculation of percentages
  ### # multipling economic weight with the genetic standard deviation to compare traits
  if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)) {
    calving_mat <- abs(EW_calving_score_mat*genetic_SD_calving_mat)
    birth_wt_mat <- abs(EW_birthwt_mat*genetic_SD_BW_mat)
    wean_wt_mat <- abs(EW_Weantwt_mat*genetic_SD_Wean_wt_mat)
  }

  calving_dir <- abs(EW_calving_score_dir*genetic_SD_calving_dir)
  birth_wt_dir <- abs(EW_birthwt_dir*genetic_SD_BW_dir)
  wean_wt_dir <- abs(EW_Weantwt_dir*genetic_SD_Wean_wt_dir)
  ACCW <- abs(EW_ACCW*genetic_SD_ACCW)
  fleshiness <- (EW_fleshiness*gemetic_SD_fleshiness)
  fat <- (EW_fat*genetic_SD_fat)


  ### # Transform in percentage
  if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)) {
    #for functional traits (production system 1 requires maternal and direct components of traits)
    sum_functional <- sum(calving_mat, calving_dir, birth_wt_mat, birth_wt_dir, wean_wt_mat, wean_wt_dir)
    calving_mat_percentage <- (calving_mat/sum_functional)*100
    birthwt_mat_percentage <- (birth_wt_mat/sum_functional)*100
    weanwt_mat_percentage <- (wean_wt_mat/sum_functional)*100
    calving_dir_percentage <- (calving_dir/sum_functional)*100
    birthwt_dir_percentage <- (birth_wt_dir/sum_functional)*100
    weanwt_dir_percentage <- (wean_wt_dir/sum_functional)*100


    # for combining functional and carcass traits - not applicable to export calves
    sum_combined <- sum(calving_mat, calving_dir, birth_wt_mat, birth_wt_dir, wean_wt_mat, wean_wt_dir, ACCW, fleshiness, fat)
    calving_ease_perc_mat_comb <- (calving_mat/sum_combined)*100
    calving_ease_perc_dir_comb <- (calving_dir/sum_combined)*100
    birth_weight_perc_mat_comb <- (birth_wt_mat/sum_combined)*100
    birth_weight_perc_dir_comb <- (birth_wt_dir/sum_combined)*100
    wean_wt_perc_mat_comb <- (wean_wt_mat/sum_combined)
    wean_wt_perc_dir_comb <- (wean_wt_dir/sum_combined)
    fleshiness_percentage_comb <- (fleshiness/sum_combined)*100
    fat_percentage_comb <- (fat/sum_combined)*100
    carcass_weight_percentage_comb <- (ACCW/sum_combined)*100

  }else if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst3)) {
    sum_functional <- sum(calving_dir, birth_wt_dir, wean_wt_dir)
    calving_dir_percentage <- (calving_dir/sum_functional)*100
    birthwt_dir_percentage <- (birth_wt_dir/sum_functional)*100
    weanwt_dir_percentage <- (wean_wt_dir/sum_functional)*100

    sum_combined <- sum(calving_dir, birth_wt_dir, wean_wt_dir, ACCW, fleshiness, fat)
    calving_ease_perc_dir_comb <- (calving_dir/sum_combined)*100
    birth_weight_perc_dir_comb <- (birth_wt_dir/sum_combined)*100
    wean_wt_perc_dir_comb <- (wean_wt_dir/sum_combined)*100
    fleshiness_percentage_comb <- (fleshiness/sum_combined)*100
    fat_percentage_comb <- (fat/sum_combined)*100
    carcass_weight_percentage_comb <- (ACCW/sum_combined)*100

  }

  # for carcass traits
  sum_carcass <- sum(ACCW, fleshiness, fat) 
  carcass_wt_perc <- (ACCW/sum_carcass)*100
  carcass_conformation_perc <- (fleshiness/sum_carcass)*100
  carcass_fat_percentage <- (fat/sum_carcass)*100

  
  ### # Depending on the trait group to consider
  if(ps_traitgroup2consider == "Carcass Traits"){
    df <- data.frame(trait = c(l_constants_postprocess_beefOnbeef$name_ACCW,
                               l_constants_postprocess_beefOnbeef$name_fleshiness,
                               l_constants_postprocess_beefOnbeef$name_fat),
                     value = c(carcass_wt_perc, 
                               carcass_conformation_perc, 
                               carcass_fat_percentage))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    
    if (ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)){
       df <- data.frame(trait = c(l_constants_postprocess_beefOnbeef$name_calvease_dir,
                                  l_constants_postprocess_beefOnbeef$name_calvease_mat,
                                  l_constants_postprocess_beefOnbeef$name_birthwt_dir,
                                  l_constants_postprocess_beefOnbeef$name_birthwt_mat,
                                  l_constants_postprocess_beefOnbeef$name_weanwt_dir,
                                  l_constants_postprocess_beefOnbeef$name_weanwt_mat),
                        value = c(calving_dir_percentage, 
                                  calving_mat_percentage, 
                                  birthwt_dir_percentage, 
                                  birthwt_mat_percentage, 
                                  weanwt_dir_percentage, 
                                  weanwt_mat_percentage))
       
    }else{
      df <- data.frame(trait = c(l_constants_postprocess_beefOnbeef$name_calvease_dir,
                                 l_constants_postprocess_beefOnbeef$name_birthwt_dir,
                                 l_constants_postprocess_beefOnbeef$name_weanwt_dir),
                       value = c(calving_dir_percentage, 
                                 birthwt_dir_percentage, 
                                 weanwt_dir_percentage))
    }
    
  }else if (ps_traitgroup2consider == "Combined") {
    
    if (ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)){
       df <- data.frame(trait = c(l_constants_postprocess_beefOnbeef$name_calvease_dir,
                                  l_constants_postprocess_beefOnbeef$name_calvease_mat,
                                  l_constants_postprocess_beefOnbeef$name_birthwt_dir,
                                  l_constants_postprocess_beefOnbeef$name_birthwt_mat,
                                  l_constants_postprocess_beefOnbeef$name_weanwt_dir,
                                  l_constants_postprocess_beefOnbeef$name_weanwt_mat,
                                  l_constants_postprocess_beefOnbeef$name_ACCW,
                                  l_constants_postprocess_beefOnbeef$name_fleshiness,
                                  l_constants_postprocess_beefOnbeef$name_fat),
                        value = c(calving_ease_perc_dir_comb, 
                                  calving_ease_perc_mat_comb, 
                                  birth_weight_perc_dir_comb, 
                                  birth_weight_perc_mat_comb, 
                                  wean_wt_perc_dir_comb, 
                                  wean_wt_perc_mat_comb, 
                                  carcass_weight_percentage_comb, 
                                  fleshiness_percentage_comb, 
                                  fat_percentage_comb))
       
    }else{
      df <- data.frame(trait = c(l_constants_postprocess_beefOnbeef$name_calvease_dir,
                                 l_constants_postprocess_beefOnbeef$name_birthwt_dir,
                                 l_constants_postprocess_beefOnbeef$name_weanwt_dir,
                                 l_constants_postprocess_beefOnbeef$name_ACCW,
                                 l_constants_postprocess_beefOnbeef$name_fleshiness,
                                 l_constants_postprocess_beefOnbeef$name_fat),
                       value = c(calving_ease_perc_dir_comb, 
                                 birth_weight_perc_dir_comb, 
                                 wean_wt_perc_dir_comb, 
                                 carcass_weight_percentage_comb, 
                                 fleshiness_percentage_comb, 
                                 fat_percentage_comb))
      
      
    }
    
    
  }



  ### # Pie chart
  base_pie <- ggplot2::ggplot(df, aes(x = "" , y = value, fill = forcats::fct_inorder(trait))) +
    ggtitle(paste0("Standardized Economic Weights for ", ps_scenario),
            subtitle = ps_traitgroup2consider)+
    geom_col(width = 1) +
    coord_polar(theta = "y", start = 0 ) +
    geom_text(aes(x = 1.6, label = paste0(round(value, 0), "%")),
              position = position_stack(vjust = 0.5))+
    guides(fill = guide_legend(title = "Trait")) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(10, 0, 0, 50))
  if(ps_traitgroup2consider == "Carcass Traits"){
    piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOnbeef$colour_ACCW, 
                                                      l_constants_postprocess_beefOnbeef$colour_fleshiness, 
                                                      l_constants_postprocess_beefOnbeef$colour_fat))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)) {
      piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOnbeef$colour_calvease_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_calvease_mat,
                                                        l_constants_postprocess_beefOnbeef$colour_birthwt_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_birthwt_mat,
                                                        l_constants_postprocess_beefOnbeef$colour_weanwt_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_weanwt_mat))
    }else{
      piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOnbeef$colour_calvease_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_birthwt_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_weanwt_dir))
    }
  } else if (ps_traitgroup2consider == "Combined") {
    if(ps_prodsystem == as.character(l_constants_ewbc_input_beefOnbeef$prodsyst1)) {
      piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOnbeef$colour_calvease_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_calvease_mat,
                                                        l_constants_postprocess_beefOnbeef$colour_birthwt_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_birthwt_mat,
                                                        l_constants_postprocess_beefOnbeef$colour_weanwt_dir,
                                                        l_constants_postprocess_beefOnbeef$colour_weanwt_mat,
                                                        l_constants_postprocess_beefOnbeef$colour_ACCW,
                                                        l_constants_postprocess_beefOnbeef$colour_fleshiness,
                                                        l_constants_postprocess_beefOnbeef$colour_fat))
      
    }else{
    piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOnbeef$colour_calvease_dir,
                                                      l_constants_postprocess_beefOnbeef$colour_birthwt_dir,
                                                      l_constants_postprocess_beefOnbeef$colour_weanwt_dir,
                                                      l_constants_postprocess_beefOnbeef$colour_ACCW, 
                                                      l_constants_postprocess_beefOnbeef$colour_fleshiness,
                                                      l_constants_postprocess_beefOnbeef$colour_fat))
    }
  }
}



#' @title Create table of results depending on sire breed, dam breed or production system
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to prepare information to be plot.
#'
#' @param ps_sort_by parameter to determine how to sort the tables: sire_breed, dam_breed or production_system
#' @param ps_path_results_tbl path to results tables from initial post processing
#' @param ps_path_tbl_save path to save the results: cannot be the same path as the results tables from inital post processing
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @import ggplot2
#' @import forcats
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import gridExtra
#'
#' @export create_table_results_ewbc
create_table_results_ewbc <- function(ps_sort_by,
                                      ps_path_results_tbl,
                                      ps_path_save,
                                      pb_log,
                                      plogger = NULL){
  
  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()
  
  temp = list.files(path = ps_path_results_tbl, pattern="df_")
  results_tables <- data.frame(temp)
  
  myfiles = lapply(paste0(ps_path_results_tbl, "/", temp), read.table, sep=",")
  
  tbl_info <- NULL
  for(idx in 1:nrow(results_tables)){
    scenario_split <- unlist(strsplit(results_tables[idx,], split = "_", fixed = TRUE))
    scenario_split <- unlist(strsplit(scenario_split, split = ".", fixed = TRUE))
    sire <- scenario_split[l_constants_postprocess_beefOnbeef$string_2]
    dam <- scenario_split[l_constants_postprocess_beefOnbeef$string_3]
    production <- scenario_split[l_constants_postprocess_beefOnbeef$string_4]
    
    
    tbl_cur_info <- tibble::tibble(Sire = sire, Dam = dam, Production = production)
    if (is.null(tbl_info)){
      tbl_info <- tbl_cur_info
    } else {
      tbl_info <- dplyr::bind_rows(tbl_info, tbl_cur_info)
    }
  }
  
  sire_breeds <- unique(tbl_info$Sire)
  dam_breeds <- unique(tbl_info$Dam)
  production_system <- unique(tbl_info$Production)
  
  # ps_sort by: can be by sire breed, dam breed or production system
  
  if(ps_sort_by == "sire_breed") {
    sex_breed <- unique(tbl_info$Sire)
    sex <- "sire"
  }else if (ps_sort_by == "dam_breed") {
    sex_breed <- unique(tbl_info$Dam)
    sex <- "dam"
  } else if (ps_sort_by == "production_system") {
    sex_breed <- unique(tbl_info$Production)
    sex <- "production_system"
  }
  
  #To make sure the strings are correctly found in the scenario name rather than from elsewhere in the table
  for(idx in 1:length(sex_breed)){
    if(ps_sort_by == "production_system"){
      breed <- paste0("_", sex_breed[idx], "_")
    } else if (ps_sort_by == "sire_breed"){
      breed <- paste0(sex_breed[idx], "_")
    } else {
      breed <- paste0("_", sex_breed[idx])
    }
    
    
    tbl_sex_breed <- myfiles[grep(breed, myfiles)]
    tbl_breed <- NULL
    
    
    
    for (jdx in 1:length(tbl_sex_breed)){
      tbl_current_breed <- as.data.frame(tbl_sex_breed[jdx])
      colnames(tbl_current_breed) <- tbl_current_breed[1,]
      rownames(tbl_current_breed) <- tbl_current_breed[,1]
      tbl_current_breed <- tbl_current_breed[-1,]
      tbl_current_breed <- tbl_current_breed[,-1]
      
      if(is.null(tbl_breed)){
        tbl_breed <- tbl_current_breed
      } else {
        tbl_breed <- dplyr::bind_rows(tbl_breed, tbl_current_breed[2,])
      }
    }
    
    
    tbl_breed$Production_system <- NA
    tbl_breed$SirexDam <- NA
    
    for(n in 2:nrow(tbl_breed)){
      column_split <- unlist(strsplit(row.names(tbl_breed)[n], "_", fixed = TRUE))
      sire <- column_split[l_constants_postprocess_beefOnbeef$string_1]
      dam <- column_split[l_constants_postprocess_beefOnbeef$string_2]
      production_sys <- column_split[l_constants_postprocess_beefOnbeef$string_3]
      tbl_breed$SirexDam[n] <- paste0(sire, "_", dam)
      tbl_breed$Production_system[n] <- production_sys
    }
    
    
    tbl_breed <- tbl_breed %>%
      dplyr::relocate("SirexDam") %>%
      dplyr::relocate("Production_system")
    
    tbl_breed[is.na(tbl_breed)] <- "-"
    tbl_breed <- tbl_breed[order((tbl_breed$Production_system)), ]
    row.names(tbl_breed) <- NULL
    
    pdf(paste0(ps_path_save,"/results_tbl_",sex,"_",breed,".pdf"), height=11, width=30)
    gridExtra::grid.table(tbl_breed)
    dev.off()
  }
  
}



#' @title Extract economic weights from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the economic weights.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_path_2outputfile_run1 path to output file from run 1 of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_aggregate_results result tibble with economic weights and units
#'
#' @export extract_ewdc
extract_ewdc <- function(ps_path_2outputfile,
                         ps_path_2outputfile_run1,
                         ps_output_statement,
                         ps_output_search_pattern,
                         ps_sirebreed,
                         ps_dambreed,
                         ps_prodsystem,
                         ps_marketchannel,
                         ps_path_directory2create,
                         pb_log,
                         plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_ewdc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_path_2outputfile_run1: ',ps_path_2outputfile_run1, '\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel,'\n',
                           ' * ps_path_directory2create: ',ps_path_directory2create,'\n'))
  }


  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- read_file_input(ps_input_file = ps_output_statement,
                                          pb_log = pb_log,
                                          plogger = lgr)


  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log = pb_log,
                                plogger = lgr)


  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()
  l_constants_progeny_beefOndairy<- get_constants_progeny_beefOndairy()

  ### # Extract the economic weight results coming from ECOWEIGHT output
  ### # Marginal economic values are taken from the section 3.11 for beef-on-dairy
  ### # explanations under https://qualitasag.atlassian.net/wiki/spaces/ZWS/pages/2965569565/20220728+--+Weekly+Meeting+Projekt+Gesamtzuchtwert+mit+Produktionsmodellen
  vec_ecow_result_EW <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                       ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_marginal_EW,],
                                       ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_direct_maternal_EW,],
                                       pb_log = pb_log,
                                       plogger = lgr)
  
  ### # Get the value
  tbl_result_ew <- NULL
  for(idx in 1:nrow(tbl_search[l_constants_postprocess_beefOndairy$ew_results,])){
    n_cur_ew <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_EW,
                                 ps_statement2search = tbl_search$SearchPattern[idx],
                                 ps_line2get = tbl_search$IndexOffset[idx],
                                 ps_splitby = "        ",
                                 pb_log = pb_log,
                                 plogger = lgr)
    n_cur_ew <- as.numeric(n_cur_ew[l_constants_postprocess_beefOndairy$string_EW_value])
    tbl_cur_ew <- tibble::tibble(Trait = tbl_search$Trait[idx], EconomicValue = n_cur_ew)
    if (is.null(tbl_result_ew)){
      tbl_result_ew <- tbl_cur_ew
    } else {
      tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_cur_ew)
    }
  }
  
  
 if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
   
   ### # NOT REQUIRED FOR EXPORT CALVES
   ### # Average fattening length is required for calculating the economic weight for age adjusted carcass weight: EW ADG/days fattening
   avg_fattening_length <- extract_avg_fattening_length(ps_path_2outputfile = ps_path_2outputfile,
                                                        ps_tbl_output_statement = tbl_output_statement,
                                                        ps_tbl_output_search_pattern = tbl_search,
                                                        ps_prodsystem = ps_prodsystem,
                                                        pb_log = pb_log,
                                                        plogger = lgr)
     
   ### # Calculation of Economic Weight for age adjusted carcass weight 
   ### # We need to convert the economic values of average daily gain (ADG):
   ### # Divide the economic weight for ADG by the total number of days of fattening.
   ### # This gives the economic value (EV) per gram increase in carcass weight → multiply by 1000 to get EV per kg increase in carcass weight 
   ### # Conversion with dressing percentage to move from live weight at slaughter to carcass weight
   AASW_EW <- (tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_ADG])/(avg_fattening_length)*1000
   ACCW <- AASW_EW*l_constants_postprocess_beefOndairy$avg_dressing
   tbl_ACCW <- tibble::tibble(Trait = "EWAgeCorrectedCarcassWeight", EconomicValue = ACCW)
   tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_ACCW)
 }
  
  
  ### # Read csv-file containing mean and standarddeviation of raw and transformed phenotype for calving score
  tbl_mean_sd <- readr::read_delim(file = file.path(ps_path_directory2create,
                                                    paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystem,"_",ps_marketchannel, collapse = ""),
                                                    "mean_sd_calvingscore.csv"), 
                                   delim = ",")
  # get the economic weight out of the result file from ECOWEIGHT (EWDC)
  EW_calving <- tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving]
  
  # economic weight per genetic standardeviation based on the raw phenotype scale
  ew_sd <- as.numeric(EW_calving)*tbl_mean_sd$sd_raw_calvscore/l_constants_postprocess_beefOndairy$calving_t_delta
  # economic weight per genetic standardeviation based on the transformed phenotype scale
  ew_u <- -(ew_sd/tbl_mean_sd$sd_transform_calvscore)
  
  # Add transformed EW for calving score to the EW table
  tbl_transformed <- tibble::tibble(Trait = "EWCalvingPerformanceTransform", EconomicValue = ew_u)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_transformed)
  
  
  ### # Get the value for gestation length based on the profit
  tbl_profit_gestlength <- extract_gestlength_ecovalue_ewdc(ps_path_2outputfile_run0 = ps_path_2outputfile,
                                                            ps_path_2outputfile_run1 = ps_path_2outputfile_run1,
                                                            ps_tbl_output_statement = tbl_output_statement,
                                                            ps_tbl_output_search_pattern = tbl_search,
                                                            pl_constants_postprocess_beefOndairy = l_constants_postprocess_beefOndairy,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_profit_gestlength)
  
  
  # Build a tibble
  traits <-  c("Calving_performance",
               "Calving_performance_transformed",
               "Birth_weight",
               "Gestation_length",
               "Age_adjusted_carcass_weight",
               "Mean_class_fleshiness",
               "Mean_class_fat")
  
    
    if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
       tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                                 EW = c(round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving], digits = 2),
                                                        round(ew_u, digits = 2),
                                                        round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_birthwt], digits = 2),
                                                        round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_gestation_length],digits = 2),
                                                        round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_ACCW], digits = 2),
                                                        round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_fleshiness], digits = 2),
                                                        round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_fat], digits = 2)),
                                                 EW_unit = c("CHF/0.01 score",
                                                             "CHF/0.01 transformed score",
                                                             "CHF/kg",
                                                             "CHF/day",
                                                             "CHF/kg",
                                                             "CHF/0.01 score",
                                                             "CHF/0.01 score"))
    }else{
      tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                              EW = c((round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving], digits = 2)),
                                                     round(ew_u, digits = 2),
                                                     round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_birthwt], digits = 2), 
                                                     round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_gestation_length_export],digits = 2),
                                                     NA, 
                                                     NA, 
                                                     NA),
                                              EW_unit = c("CHF/0.01 score",
                                                          "CHF/0.01 transformed score",
                                                          "CHF/kg",
                                                          "CHF/day",
                                                          "CHF/kg",
                                                          "CHF/0.01 score",
                                                          "CHF/0.01 score"))
    }
      
      
    return(tbl_aggregate_results)
  
} 


#' @title Extract or calculate population mean from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract the population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystem production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_aggregate_results result tibble with population mean
#'
#' @export extract_popmean_ewdc
extract_popmean_ewdc <- function(ps_path_2outputfile,
                                 ps_output_statement,
                                 ps_output_search_pattern,
                                 ps_sirebreed,
                                 ps_dambreed,
                                 ps_prodsystem,
                                 ps_marketchannel,
                                 ps_path_directory2create,
                                 pb_log,
                                 plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_popmean_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_popmean_ewdc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystem: ',ps_prodsystem, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel,'\n',
                           ' * ps_path_directory2create: ',ps_path_directory2create,'\n'))
  }
  
  
  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- read_file_input(ps_input_file = ps_output_statement,
                                          pb_log = pb_log,
                                          plogger = lgr)
  
  
  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log = pb_log,
                                plogger = lgr)
  
  
  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()
  l_constants_progeny_beefOndairy<- get_constants_progeny_beefOndairy()
  
  
  ### # Extract the population mean of the results coming from ECOWEIGHT output 
  ### # For ConventionalVeal and ConvenionalBeef :calving performance, fleshiness, fat
  ### # Whereas for Export only calving score and birth weight
  tbl_result_misc <- extract_calv_flesh_fat_popmean(ps_path_2outputfile = ps_path_2outputfile,
                                                    ps_tbl_output_statement = tbl_output_statement,
                                                    ps_tbl_output_search_pattern = tbl_search,
                                                    pl_constants_postprocess = l_constants_postprocess_beefOndairy,
                                                    ps_marketchannel = ps_marketchannel,
                                                    ps_prodsystem = ps_prodsystem,
                                                    pb_log = pb_log,
                                                    plogger = lgr)
  tbl_result_mean <- tbl_result_misc
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for birth weight
  tbl_result_birthwt <- extract_birth_weaning_weight_popmean(ps_path_2outputfile = ps_path_2outputfile,
                                                             ps_tbl_output_statement = tbl_output_statement,
                                                             ps_tbl_output_search_pattern = tbl_search,
                                                             pl_constants_postprocess = l_constants_postprocess_beefOndairy,
                                                             ps_marketchannel = ps_marketchannel,
                                                             ps_prodsystem = ps_prodsystem,
                                                             pb_log = pb_log,
                                                             plogger = lgr)
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_birthwt)
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for gestation length
  tbl_result_gestation <- extract_gestlength_popmean_ewdc(ps_path_2outputfile = ps_path_2outputfile,
                                                          ps_tbl_output_statement = tbl_output_statement,
                                                          ps_tbl_output_search_pattern = tbl_search,
                                                          pl_constants_postprocess_beefOndairy = l_constants_postprocess_beefOndairy,
                                                          pl_constants_progeny_beefOndairy = l_constants_progeny_beefOndairy,
                                                          ps_marketchannel = ps_marketchannel,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_gestation)
  
  
  if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
    ### # Extract the part of interest of the results coming from ECOWEIGHT output for carcass weight
    tbl_result_slaughter <- extract_slaughterweight_popmean(ps_path_2outputfile = ps_path_2outputfile,
                                                            ps_tbl_output_statement = tbl_output_statement,
                                                            ps_tbl_output_search_pattern = tbl_search,
                                                            pl_constants_postprocess = l_constants_postprocess_beefOndairy,
                                                            ps_marketchannel = ps_marketchannel,
                                                            ps_prodsystem = ps_prodsystem,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_slaughter)
    
    ### # Extract the part of interest of the results coming from ECOWEIGHT output for average daily gain
    ### # Calculation of carcass weight average are not included in results of ECOWEIGHT output so needs to be calculated manually
    # e.g. for heifers: Slaughter wt = (Daily gain of heifers in fattening * days in fattening) + average weight of female calves at end of the rearing period
    # To make it the average for carcass weight, we then need to multiply by the dressing proportion for heifers and males then average the carcass weight female and male to find one value
    tbl_result_adg <- extract_adgfattening_popmean_ewdc(ps_path_2outputfile = ps_path_2outputfile,
                                                        ps_tbl_output_statement = tbl_output_statement,
                                                        ps_tbl_output_search_pattern = tbl_search,
                                                        pl_constants_postprocess_beefOndairy = l_constants_postprocess_beefOndairy,
                                                        pl_constants_progeny_beefOndairy = l_constants_progeny_beefOndairy,
                                                        ps_marketchannel = ps_marketchannel,
                                                        pb_log = pb_log,
                                                        plogger = lgr)
    tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_adg)
    
    
    ### # Average daily gain during fattening averaged over male and female
    ADG_fattening <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_ADGm])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_ADGf]))/2
    
    ### # Extract the part of interest of the results coming from ECOWEIGHT output for weight at the end of rearing
    tbl_result_rearing <- extract_rearingweight_popmean_ewdc(ps_path_2outputfile = ps_path_2outputfile,
                                                             ps_tbl_output_statement = tbl_output_statement,
                                                             ps_tbl_output_search_pattern = tbl_search,
                                                             pl_constants_postprocess_beefOndairy = l_constants_postprocess_beefOndairy,
                                                             pl_constants_progeny_beefOndairy = l_constants_progeny_beefOndairy,
                                                             ps_marketchannel = ps_marketchannel,
                                                             pb_log = pb_log,
                                                             plogger = lgr)
    tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_rearing)
    
    Rearing_wt <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_rearingwt_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_rearingwt_f]))/2
    
    ### # Calculate average slaughter weight
    avg_fattening_length <- extract_avg_fattening_length(ps_path_2outputfile = ps_path_2outputfile,
                                                         ps_tbl_output_statement = tbl_output_statement,
                                                         ps_tbl_output_search_pattern = tbl_search,
                                                         ps_prodsystem = ps_prodsystem,
                                                         pb_log = pb_log,
                                                         plogger = lgr)
    
    avg_slaughter_wt <- (ADG_fattening*avg_fattening_length) + Rearing_wt
    ### # Convert to average carcass weight by multiplying by average dressing proportion (0.58 for bulls and 0.54 for heifers = 0.56)
    avg_carcass_wt <- avg_slaughter_wt*l_constants_postprocess_beefOndairy$avg_dressing
    ### # Add result to mean table
    tbl_avg_carcasswt <- tibble::tibble(Trait = "Carcass_wt", MeanValue = avg_carcass_wt)
    
    tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_avg_carcasswt)
    
    
  }  
  
  
  ### # Calculation of some average values
  if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
    Fleshiness <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_flesh_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_flesh_f]))/2
    Fat <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_fat_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_fat_f]))/2
    Birth_weight <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_f]))/2
    Calving_score <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_f]))/2
  }else{
    Birth_weight <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_exportm])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_exportf]))/2
    Calving_score <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_exportm])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_exportf]))/2
  }
  
  
  ### # Get mean value of calving scores
  ### # Read csv-file containing mean and standarddeviation of raw and transformed phenotype for calving score
  tbl_mean_sd <- readr::read_delim(file = file.path(ps_path_directory2create,
                                                    paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystem,"_",ps_marketchannel, collapse = ""),
                                                    "mean_sd_calvingscore.csv"), 
                                   delim = ",")
  
  
  # Build a tibble
  traits <-  c("Calving_performance",
               "Calving_performance_transformed",
               "Birth_weight",
               "Gestation_length",
               "Age_adjusted_carcass_weight",
               "Mean_class_fleshiness",
               "Mean_class_fat")
  
  if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf){
    tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                            Population_mean = c(round(Calving_score, digits = 2),
                                                                round(tbl_mean_sd$mean_transform_calvscore, digits = 2),
                                                                round(Birth_weight, digits = 2),
                                                                round(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_gestlen],digits = 2),
                                                                round(avg_carcass_wt, digits = 2),
                                                                round(Fleshiness, digits = 2),
                                                                round(Fat, digits = 2)))
  }else{
    tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                            Population_mean = c(round(Calving_score, digits = 2),
                                                                round(tbl_mean_sd$mean_transform_calvscore, digits = 2),
                                                                round(Birth_weight,digits = 2),
                                                                round(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_gestlen_export],digits = 2),
                                                                NA,
                                                                NA,
                                                                NA))
  }
  
  
  return(tbl_aggregate_results)
  
  
}  


#' @title Extract gestation length from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract gestation length population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess_beefOndairy constants of postprocessing beefOndairy
#' @param pl_constants_progeny_beefOndairy constants of progeny beefOndairy
#' @param ps_marketchannel market channel
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_gestation tibble results with gestation length population mean
#'
#' @export extract_gestlength_popmean_ewdc
extract_gestlength_popmean_ewdc <- function(ps_path_2outputfile,
                                            ps_tbl_output_statement,
                                            ps_tbl_output_search_pattern,
                                            pl_constants_postprocess_beefOndairy,
                                            pl_constants_progeny_beefOndairy,
                                            ps_marketchannel,
                                            pb_log,
                                            plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_gestlength_popmean_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_gestlength_popmean_ewdc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess_beefOndairy \n',
                           ' * pl_constants_progeny_beefOndairy \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n'))
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for gestation length
  vec_ecow_result_gestation <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                              ps_start_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_INPUT11,],
                                              ps_end_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_INPUT23,],
                                              pb_log = pb_log,
                                              plogger = lgr)
  
  tbl_result_gestation<- NULL
  tbl_search_gestation <- ps_tbl_output_search_pattern[pl_constants_postprocess_beefOndairy$search_gestation,]
  n_cur_gestation <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_gestation,
                                      ps_statement2search = tbl_search_gestation$SearchPattern,
                                      ps_line2get = tbl_search_gestation$IndexOffset,
                                      ps_splitby = "    ",
                                      pb_log = pb_log,
                                      plogger = lgr)
  n_cur_gestation <- as.numeric(n_cur_gestation[pl_constants_postprocess_beefOndairy$string_2])
  
  tbl_cur_result_gestation <- tibble::tibble(Trait = tbl_search_gestation$Trait, MeanValue = n_cur_gestation)
  if (is.null(tbl_result_gestation)){
    tbl_result_gestation <- tbl_cur_result_gestation
  } else {
    tbl_result_gestation <- dplyr::bind_rows(tbl_result_gestation, tbl_cur_result_gestation)
  }
  
  
  return(tbl_result_gestation)
  
}


#' @title Extract gestation length economic value from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract gestation length economic value.
#'
#' @param ps_path_2outputfile_run0 path to output file run 0 of ECOWEIGHT
#' @param ps_path_2outputfile_run1 path to output file run 1 of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess_beefOndairy constants of postprocessing beefOndairy
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_profit_gestlength tibble results with gestation length economic value
#'
#' @export extract_gestlength_ecovalue_ewdc
extract_gestlength_ecovalue_ewdc <- function(ps_path_2outputfile_run0,
                                             ps_path_2outputfile_run1,
                                             ps_tbl_output_statement,
                                             ps_tbl_output_search_pattern,
                                             pl_constants_postprocess_beefOndairy,
                                             pb_log,
                                             plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_gestlength_ecovalue_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_gestlength_ecovalue_ewdc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile_run0', ps_path_2outputfile_run0, '\n',
                           ' * ps_path_2outputfile_run1: ',ps_path_2outputfile_run1, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess_beefOndairy \n'))
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output run0 for gestation length 
  vec_ecow_result_gestation_run0 <- extract_result(ps_path_2outputfile = ps_path_2outputfile_run0,
                                                   ps_start_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_profit,],
                                                   ps_end_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_marginal_EW,],
                                                   pb_log = pb_log,
                                                   plogger = lgr)
  
  tbl_result_profit<- NULL
  tbl_search_profit <- ps_tbl_output_search_pattern[pl_constants_postprocess_beefOndairy$search_profit,]
  n_cur_profit <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_gestation_run0,
                                   ps_statement2search = tbl_search_profit$SearchPattern,
                                   ps_line2get = tbl_search_profit$IndexOffset,
                                   ps_splitby = "    ",
                                   pb_log = pb_log,
                                   plogger = lgr)
  n_cur_profit <- as.numeric(n_cur_profit[pl_constants_postprocess_beefOndairy$string_2])
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output run1 for gestation length 
  vec_ecow_result_gestation_run1 <- extract_result(ps_path_2outputfile = ps_path_2outputfile_run1,
                                                   ps_start_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_profit,],
                                                   ps_end_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_marginal_EW,],
                                                   pb_log = pb_log,
                                                   plogger = lgr)
  
  n_cur_profit_run1 <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_gestation_run1,
                                   ps_statement2search = tbl_search_profit$SearchPattern,
                                   ps_line2get = tbl_search_profit$IndexOffset,
                                   ps_splitby = "    ",
                                   pb_log = pb_log,
                                   plogger = lgr)
  n_cur_profit_run1 <- as.numeric(n_cur_profit_run1[pl_constants_postprocess_beefOndairy$string_2])
  
  
  ### # Calculate the difference of the profit between run0 and run1 for gestation length
  diff_profit_gestlength <- n_cur_profit - n_cur_profit_run1
  
  
  tbl_profit_gestlength <- tibble::tibble(Trait = "EWGestationLength", EconomicValue = diff_profit_gestlength)
  
  
  return(tbl_profit_gestlength)
  
  

}


#' @title Extract average daily gain during fattening from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract average daily gain population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess_beefOndairy constants of postprocessing beefOndairy
#' @param pl_constants_progeny_beefOndairy constants of progeny beefOndairy
#' @param ps_marketchannel market channel
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_adg tibble results with average daily gain population mean
#'
#' @export extract_adgfattening_popmean_ewdc
extract_adgfattening_popmean_ewdc <- function(ps_path_2outputfile,
                                              ps_tbl_output_statement,
                                              ps_tbl_output_search_pattern,
                                              pl_constants_postprocess_beefOndairy,
                                              pl_constants_progeny_beefOndairy,
                                              ps_marketchannel,
                                              pb_log,
                                              plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_adgfattening_popmean_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_adgfattening_popmean_ewdc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess_beefOndairy \n',
                           ' * pl_constants_progeny_beefOndairy \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n'))
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for average daily gain
  vec_ecow_result_adg <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                              ps_start_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_INPUT15,],
                                              ps_end_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_INPUT11,],
                                              pb_log = pb_log,
                                              plogger = lgr)
  tbl_result_adg <- NULL
  tbl_search_adg <- ps_tbl_output_search_pattern[pl_constants_postprocess_beefOndairy$search_carcass,]
  for (idx in 1:nrow(tbl_search_adg)){
    n_cur_adg <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_adg,
                                  ps_statement2search = tbl_search_adg$SearchPattern[idx],
                                  ps_line2get = tbl_search_adg$IndexOffset[idx],
                                  ps_splitby = "    ",
                                  pb_log = pb_log,
                                  plogger = lgr)
    # Get the value for crossbreds
    n_cur_adg <- as.numeric(n_cur_adg[pl_constants_postprocess_beefOndairy$string_3])
    tbl_cur_result_adg <- tibble::tibble(Trait = tbl_search_adg$Trait[idx], MeanValue = n_cur_adg)
    if (is.null(tbl_cur_result_adg)){
      tbl_result_adg <- tbl_cur_result_adg
    } else {
      tbl_result_adg <- dplyr::bind_rows(tbl_result_adg, tbl_cur_result_adg)
    }
  }
  
  
  return(tbl_result_adg)
  
}


#' @title Extract weight at the end of rearing from the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function extract weight at the end of rearing population mean.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_tbl_output_statement tibble with output statement
#' @param ps_tbl_output_search_pattern tibble with the search patterns
#' @param pl_constants_postprocess_beefOndairy constants of postprocessing beefOndairy
#' @param pl_constants_progeny_beefOndairy constants of progeny beefOndairy
#' @param ps_marketchannel market channel
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import readr
#'
#' @return tbl_result_rearing tibble results with weight at the end of rearing population mean
#'
#' @export extract_rearingweight_popmean_ewdc
extract_rearingweight_popmean_ewdc <- function(ps_path_2outputfile,
                                               ps_tbl_output_statement,
                                               ps_tbl_output_search_pattern,
                                               pl_constants_postprocess_beefOndairy,
                                               pl_constants_progeny_beefOndairy,
                                               ps_marketchannel,
                                               pb_log,
                                               plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'extract_rearingweight_popmean_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'extract_rearingweight_popmean_ewdc',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_tbl_output_statement: ', ps_tbl_output_statement, '\n',
                           ' * ps_tbl_output_search_pattern: ', ps_tbl_output_search_pattern, '\n',
                           ' * pl_constants_postprocess_beefOndairy \n',
                           ' * pl_constants_progeny_beefOndairy \n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n'))
  }
  
  
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for rearing weight
  vec_ecow_result_rearingwt <- extract_result(ps_path_2outputfile = ps_path_2outputfile,
                                              ps_start_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_growth,],
                                              ps_end_statement2extract = ps_tbl_output_statement[pl_constants_postprocess_beefOndairy$idx_row_reproduction,],
                                              pb_log = pb_log,
                                              plogger = lgr)
  tbl_result_rearing <- NULL
  tbl_search_rearing <- ps_tbl_output_search_pattern[pl_constants_postprocess_beefOndairy$search_rearing,]
  for (idx in 1:nrow(tbl_search_rearing)){
    n_cur_rearing <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_rearingwt,
                                      ps_statement2search = tbl_search_rearing$SearchPattern[idx],
                                      ps_line2get = tbl_search_rearing$IndexOffset[idx],
                                      ps_splitby = "    ",
                                      pb_log = pb_log,
                                      plogger = lgr)
    n_cur_rearing <- as.numeric(n_cur_rearing[pl_constants_postprocess_beefOndairy$string_3])
    tbl_cur_result_rearing <- tibble::tibble(Trait = tbl_search_rearing$Trait[idx], MeanValue = n_cur_rearing)
    if (is.null(tbl_cur_result_rearing)){
      tbl_result_rearing <- tbl_cur_result_rearing
    } else {
      tbl_result_rearing <- dplyr::bind_rows(tbl_result_rearing, tbl_cur_result_rearing)
    }
  }
  
  
  return(tbl_result_rearing)
  
}


#' @title Plot pie chart of the results coming from ECOWEIGHT dairy beef cattle
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to prepare information to be plot.
#'
#' @param ps_path_2genSD path to file with genetic standard deviation
#' @param ptbl_EW_results tibble of economic weights
#' @param ps_traitgroup2consider traitgroup may be Carcass or Functional Traits
#' @param ps_scenario name of the scenario (includes sire breed, dam breed, production system and marketing channel)
#' @param ps_marketchannel marketing channel required for determining the correct genetic standard deviation to use
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @import ggplot2
#' @import forcats
#'
#' @export plot_piechart_ewdc
plot_piechart_ewdc <- function(ps_path_2genSD,
                               ptbl_EW_results,
                               ps_traitgroup2consider,
                               ps_scenario,
                               ps_marketchannel,
                               pb_log,
                               plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'plot_piechart_ewbc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'plot_piechart_ewbc',
                    paste0('Starting function with parameters:\n * ps_path_2genSD', ps_path_2genSD,'\n',
                           ' * ptbl_EW_results \n',
                           ' * ps_traitgroup2consider: ', ps_traitgroup2consider, '\n',
                           ' * ps_scenario: ', ps_scenario, '\n',
                           ' * ps_marketchannel: ',ps_marketchannel, '\n'))
  }
  
  
  ### # Read file with genetic standard deviation
  tbl_gen_SD <- read_file_input(ps_input_file = ps_path_2genSD,
                                pb_log = pb_log,
                                plogger = lgr)
  
  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()
  l_constants_progeny_beefOndairy<- get_constants_progeny_beefOndairy()
  
  
  if (ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_beef) {
    genetic_SD_ACCW <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_ACCW_adult]
    genetic_SD_fleshiness <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fleshiness_adult]
    genetic_SD_fat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fat_adult]
  } else if (ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_calf) {
    genetic_SD_ACCW <- (tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_ACCW_calf])
    genetic_SD_fleshiness <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fleshiness_calf]
    genetic_SD_fat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fat_calf]
  }
  
  genetic_SD_calving_score <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_calving_ease]
  gemetic_SD_birth_wt <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_birth_weight]
  genetic_SD_gestation <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_gestation_length]
  
  
  ### # Take economic weights from table and transform them to same unit as EBV
  EW_calving_score <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOndairy$ew_pie_calving_transform]) 
  #need to convert kg to dt to match EBV
  EW_fleshiness <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOndairy$ew_pie_fleshiness]) *100
  EW_fat <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOndairy$ew_pie_fat]) *100
  EW_ACCW <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOndairy$ew_pie_ACCW]) *100
  EW_birth_wt <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOndairy$ew_pie_birthwt])
  EW_gestlen <- as.numeric(ptbl_EW_results$EW[l_constants_postprocess_beefOndairy$ew_pie_gestlen])
  
  
  ### # Ensure the economic weight is positive using absolute value for calculation of percentages
  ### # multipling economic weight with the genetic standard deviation to compare traits
  if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf) {
    fleshiness <- abs(EW_fleshiness*genetic_SD_fleshiness)
    fat <- abs(EW_fat*genetic_SD_fat)
    carcass_weight <- abs(EW_ACCW*genetic_SD_ACCW)
  }
  calving_ease <- abs(genetic_SD_calving_score*EW_calving_score)
  birth_weight <- abs(gemetic_SD_birth_wt*EW_birth_wt)
  gest_length <- abs(genetic_SD_gestation*EW_gestlen)
  
  
  ### # Transform in percentage
  if(ps_marketchannel != l_constants_progeny_beefOndairy$export_calf) {
    #for carcass traits (not required for export)
    sum_carcass <- sum(fleshiness, fat, carcass_weight)
    fleshiness_percentage <- (fleshiness/sum_carcass)*100
    fat_percentage <- (fat/sum_carcass)*100
    carcass_weight_percentage <- (carcass_weight/sum_carcass)*100
    # for combining functional and carcass traits - not applicable to export calves
    sum_combined <- sum(calving_ease, birth_weight, gest_length, fleshiness, fat, carcass_weight)
    calving_ease_perc_comb <- (calving_ease/sum_combined)*100
    birth_weight_perc_comb <- (birth_weight/sum_combined)*100
    gest_length_perc_comb <- (gest_length/sum_combined)*100
    fleshiness_percentage_comb <- (fleshiness/sum_combined)*100
    fat_percentage_comb <- (fat/sum_combined)*100
    carcass_weight_percentage_comb <- (carcass_weight/sum_combined)*100
  }
  
  ### # Need to add gestation length when we have a solution to its calculation
  sum_functional <- sum(calving_ease, birth_weight, gest_length) 
  calving_ease_perc <- (calving_ease/sum_functional)*100
  birth_weight_perc <- (birth_weight/sum_functional)*100
  gest_length_perc <- (gest_length/sum_functional)*100
  
  ### # Depending on the trait group to consider
  if(ps_traitgroup2consider == "Carcass Traits"){
    df <- data.frame(trait = c(l_constants_postprocess_beefOndairy$name_ACCW,
                               l_constants_postprocess_beefOndairy$name_fleshiness,
                               l_constants_postprocess_beefOndairy$name_fat),
                     value = c(carcass_weight_percentage, 
                               fleshiness_percentage, 
                               fat_percentage))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    df <- data.frame(trait = c(l_constants_postprocess_beefOndairy$name_calvease_dir,
                               l_constants_postprocess_beefOndairy$name_birthwt_dir,
                               l_constants_postprocess_beefOndairy$name_gestlength),
                     value = c(calving_ease_perc, 
                               birth_weight_perc,
                               gest_length_perc))
  }else if (ps_traitgroup2consider == "Combined") {
    df <- data.frame(trait = c(l_constants_postprocess_beefOndairy$name_calvease_dir,
                               l_constants_postprocess_beefOndairy$name_birthwt_dir,
                               l_constants_postprocess_beefOndairy$name_gestlength,
                               l_constants_postprocess_beefOndairy$name_ACCW,
                               l_constants_postprocess_beefOndairy$name_fleshiness,
                               l_constants_postprocess_beefOndairy$name_fat),
                     value = c(calving_ease_perc_comb,
                               birth_weight_perc_comb, 
                               gest_length_perc_comb,
                               carcass_weight_percentage_comb, 
                               fleshiness_percentage_comb, 
                               fat_percentage_comb))
  }
  
  ### # Pie chart
  base_pie <- ggplot2::ggplot(df, aes(x = "" , y = value, fill = forcats::fct_inorder(trait))) +
    ggtitle(paste0("Standardized Economic Weights for ", ps_scenario),
            subtitle = ps_traitgroup2consider)+
    geom_col(width = 1) +
    coord_polar(theta = "y", start = 0 ) +
    geom_text(aes(x = 1.6, label = paste0(round(value, 0), "%")),
              position = position_stack(vjust = 0.5))+
    guides(fill = guide_legend(title = "Trait")) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(10, 0, 0, 50))
  
  if(ps_traitgroup2consider == "Carcass Traits"){
    piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOndairy$colour_ACCW,
                                                      l_constants_postprocess_beefOndairy$colour_fleshiness,
                                                      l_constants_postprocess_beefOndairy$colour_fat))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOndairy$colour_calvease_dir,
                                                      l_constants_postprocess_beefOndairy$colour_birthwt_dir,
                                                      l_constants_postprocess_beefOndairy$colour_gestlength))
  } else if (ps_traitgroup2consider == "Combined") {
    piechart <- base_pie + scale_fill_manual(values=c(l_constants_postprocess_beefOndairy$colour_calvease_dir,
                                                      l_constants_postprocess_beefOndairy$colour_birthwt_dir,
                                                      l_constants_postprocess_beefOndairy$colour_gestlength,
                                                      l_constants_postprocess_beefOndairy$colour_ACCW,
                                                      l_constants_postprocess_beefOndairy$colour_fleshiness,
                                                      l_constants_postprocess_beefOndairy$colour_fat))
  }
  
  
  
  
  return(piechart)
  
}



#' @title Create table of results depending on sire breed, dam breed or marketing channel
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to prepare information to be plot.
#'
#' @param ps_sort_by parameter to determine how to sort the tables: sire_breed, dam_breed or marketing_channel
#' @param ps_path_results_tbl path to results tables from initial post processing
#' @param ps_path_tbl_save path to save the results: cannot be the same path as the results tables from inital post processing
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @import ggplot2
#' @import forcats
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#' @import gridExtra
#'
#' @export create_table_results_ewdc
create_table_results_ewdc <- function(ps_sort_by,
                                      ps_path_results_tbl,
                                      ps_path_save,
                                      pb_log,
                                      plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'create_table_results_ewdc.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'create_table_results_ewdc',
                    paste0('Starting function with parameters:\n * ps_sort_by', ps_sort_by, '\n',
                           ' * ps_path_results_tbl: ', ps_path_results_tbl, '\n',
                           ' * ps_path_save: ', ps_path_save, '\n'))
  }
  
  
  ### # Get the usefull constant
  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()
  
  
  ### # List all .csv file in ps_path_results_tbl
  temp = list.files(path = ps_path_results_tbl, pattern="df_")
  results_tables <- data.frame(temp)
  
  
  ### # Look content all .csv file
  myfiles = lapply(paste0(ps_path_results_tbl, "/", temp), read.table, sep=",")
  
  
  ### # Extract information like sire, dam, marketing from the name of the files
  tbl_info <- NULL
  for(idx in 1:nrow(results_tables)){
    scenario_split <- unlist(strsplit(results_tables[idx,], split = "_", fixed = TRUE))
    scenario_split <- unlist(strsplit(scenario_split, split = ".", fixed = TRUE))
    sire <- scenario_split[l_constants_postprocess_beefOndairy$string_2]
    dam <- scenario_split[l_constants_postprocess_beefOndairy$string_3]
    marketing <- scenario_split[l_constants_postprocess_beefOndairy$string_5]
    
    
    tbl_cur_info <- tibble::tibble(Sire = sire, Dam = dam, Marketing = marketing)
    if (is.null(tbl_info)){
      tbl_info <- tbl_cur_info
    } else {
      tbl_info <- dplyr::bind_rows(tbl_info, tbl_cur_info)
    }
  }
  
  
  ### # Find the criteria to sort
  if(ps_sort_by == "sire_breed") {
    criteria2sort <- unique(tbl_info$Sire)
    criteria <- "sire"
  }else if (ps_sort_by == "dam_breed") {
    criteria2sort <- unique(tbl_info$Dam)
    criteria <- "dam"
  } else if (ps_sort_by == "marketing_channel") {
    criteria2sort <- unique(tbl_info$Marketing)
    criteria <- "marketing"
  }
  
  
  for(idx in 1:length(criteria2sort)){
    breed <- criteria2sort[idx]
    # Get all files according to the criteria2sort
    tbl_criteria2sort <- myfiles[grep(breed, myfiles)]
    tbl_breed <- NULL
    
    # Loop over each file
    for (jdx in 1:length(tbl_criteria2sort)){
      tbl_current_breed <- as.data.frame(tbl_criteria2sort[jdx])
      colnames(tbl_current_breed) <- tbl_current_breed[1,]
      rownames(tbl_current_breed) <- tbl_current_breed[,1]
      tbl_current_breed <- tbl_current_breed[-1,]
      tbl_current_breed <- tbl_current_breed[,-1]
      
      if(is.null(tbl_breed)){
        tbl_breed <- tbl_current_breed
      } else {
        # Only put the value because the unit are already in the overall tibble tbl_breed
        tbl_breed <- dplyr::bind_rows(tbl_breed, tbl_current_breed[2,])
      }
    }
    
    
    # Formating with marketing_channel and sirexdam
    tbl_breed$Marketing_channel <- NA
    tbl_breed$SirexDam <- NA
    
    for(n in 2:nrow(tbl_breed)){
      column_split <- unlist(strsplit(row.names(tbl_breed)[n], "_", fixed = TRUE))
      sire <- column_split[l_constants_postprocess_beefOndairy$string_1]
      dam <- column_split[l_constants_postprocess_beefOndairy$string_2]
      marketing_ch <- column_split[l_constants_postprocess_beefOndairy$string_4]
      tbl_breed$SirexDam[n] <- paste0(sire, "_", dam)
      tbl_breed$Marketing_channel[n] <- marketing_ch
    }
    
    
    tbl_breed <- tbl_breed %>% dplyr::relocate("SirexDam") %>%
      dplyr::relocate("Marketing_channel")
    
    tbl_breed[is.na(tbl_breed)] <- "-"
    tbl_breed <- tbl_breed[order((tbl_breed$Marketing_channel)), ]
    row.names(tbl_breed) <- NULL
    
    pdf(paste0(ps_path_save,"/results_tbl_",criteria,"_",breed,".pdf"), height=11, width=20)
    gridExtra::grid.table(tbl_breed)
    dev.off()
  }
  
}
