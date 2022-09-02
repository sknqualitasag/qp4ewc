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
#' @param ps_path_tbl_save path tp the directory for saving the results output pdfs
#' @param ps_scenario name of scenario (sire_dam_system_marketingchannel)
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#'
#' @export post_process_ewbc_output
post_process_ewbc_output <- function(ps_path_2outputfile,
                                     ps_output_statement,
                                     ps_output_search_pattern,
                                     ps_path_tbl_save,
                                     ps_scenario,
                                     ps_sirebreed,
                                     ps_dambreed,
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
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_path_tbl_save: ', ps_path_tbl_save, '\n'))
  }


  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- read_file_input(ps_output_statement,
                                          pb_log,
                                          plogger = lgr)


  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log,
                                plogger = lgr)


  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()

  #we need to use the name of the file to determine the porduction system (1 or 3)
  #if there is a 1 in the scenario (e.g. AN_AN_1_Natura_Beef), the output will be one and production system is 1. If the output is integer(0) it is production system 3.
  #this is important because the results files from ecoweight are different for system 1 and 3.

  if(length(grep(pattern = "1", ps_scenario, fixed = TRUE)) > 0 ) {
    production_system <- "1"
  }else{
    production_system <- "3"
  }
  # ****************************************************************************
  ## ---- Economic weight results ----
  # ****************************************************************************
  ### # Extract the part of interest of the results coming from ECOWEIGHT output
  vec_ecow_result_EW <- extract_result(ps_path_2outputfile,
                                       ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_EW_d_m,],
                                       ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_EW_relative,],
                                       pb_log,
                                       plogger = lgr)


  ### # Get the value
  tbl_result_ew <- NULL
  for(idx in 1:nrow(tbl_search[l_constants_postprocess_beefOnbeef$search_ew,])){
    n_cur_ew <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_EW,
                                 ps_statement2search = tbl_search$SearchPattern[idx],
                                 ps_line2get = tbl_search$IndexOffset[idx],
                                 ps_splitby = "  ",
                                 pb_log,
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

  if(production_system == 1){
    ps_end_statement2extract_fat_f = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_fattened_heifers1,]
  }else{
    ps_end_statement2extract_fat_f = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_fattened_heifers3,]
  }
#Fattening length for heifers
  vec_ecow_result_fat_f <- extract_result(ps_path_2outputfile,
                                        ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_fattened_heifers,],
                                        ps_end_statement2extract = ps_end_statement2extract_fat_f,
                                        pb_log,
                                        plogger = lgr)

  tbl_search_fat_f <- tbl_search[l_constants_postprocess_beefOnbeef$idx_row_fat_length_f,]
    n_cur_ew_fat_f <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat_f,
                                     ps_statement2search = tbl_search_fat_f$SearchPattern,
                                     ps_line2get = tbl_search_fat_f$IndexOffset,
                                     ps_splitby = "    ",
                                     pb_log,
                                     plogger = lgr)
    n_cur_ew_fat_f <- n_cur_ew_fat_f[n_cur_ew_fat_f != ""]
    length_fattening_f <- as.numeric(n_cur_ew_fat_f[l_constants_postprocess_beefOnbeef$string_3])

#Fattening length for bulls
vec_ecow_result_fat_m <- extract_result(ps_path_2outputfile,
                                            ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_fattened_bulls,],
                                            ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_fattened_heifers,],
                                            pb_log,
                                            plogger = lgr)

    tbl_search_fat_m <- tbl_search[l_constants_postprocess_beefOnbeef$idx_row_fat_length_m,]
    n_cur_ew_fat_m <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat_m,
                                       ps_statement2search = tbl_search_fat_m$SearchPattern,
                                       ps_line2get = tbl_search_fat_m$IndexOffset,
                                       ps_splitby = "    ",
                                       pb_log,
                                       plogger = lgr)
    n_cur_ew_fat_m <- n_cur_ew_fat_m[n_cur_ew_fat_m != ""]
    length_fattening_m <- as.numeric(n_cur_ew_fat_m[l_constants_postprocess_beefOnbeef$string_3])

avg_length_fat <- (length_fattening_f + length_fattening_m)/2


  # ****************************************************************************
  ## ---- Average values ----
  # ****************************************************************************
  ### # Extract the part of interest of the results coming from ECOWEIGHT output for proportion of males and females
  ### # How many females are kept for replacement will affect how to calculate the average trait values at slaughter
  vec_ecow_result_prop <- extract_result(ps_path_2outputfile,
                                        ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_structure,],
                                        ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_growth,],
                                        pb_log,
                                        plogger = lgr)
  tbl_result_prop <- NULL
    tbl_search_prop <- tbl_search[l_constants_postprocess_beefOnbeef$idx_row_prop,]
  for (idx in 1:nrow(tbl_search_prop)){
    n_cur_prop <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_prop,
                                     ps_statement2search = tbl_search_prop$SearchPattern[idx],
                                     ps_line2get = tbl_search_prop$IndexOffset[idx],
                                     ps_splitby = ":",
                                     pb_log,
                                     plogger = lgr)
    n_cur_prop <- as.numeric(n_cur_prop[l_constants_postprocess_beefOnbeef$string_2])
    tbl_cur_result_prop <- tibble::tibble(Trait = tbl_search_prop$Trait[idx], MeanValue = n_cur_prop)
    if (is.null(tbl_result_prop)){
      tbl_result_prop <- tbl_cur_result_prop
    } else {
      tbl_result_prop <- dplyr::bind_rows(tbl_result_prop, tbl_cur_result_prop)
    }
  }


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for calving performance, fleshiness, fat
  vec_ecow_result_misc <- extract_result(ps_path_2outputfile,
                                         ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_misc,],
                                         ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_nutrition,],
                                         pb_log,
                                         plogger = lgr)
  tbl_result_misc <- NULL
  tbl_search_misc <- tbl_search[l_constants_postprocess_beefOnbeef$search_misc,]
  for (idx in 1:nrow(tbl_search_misc)){
    n_cur_misc <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_misc,
                                   ps_statement2search = tbl_search_misc$SearchPattern[idx],
                                   ps_line2get = tbl_search_misc$IndexOffset[idx],
                                   ps_splitby = ":",
                                   pb_log,
                                   plogger = lgr)
    n_cur_misc <- as.numeric(n_cur_misc[l_constants_postprocess_beefOnbeef$string_2])
    tbl_cur_result_misc <- tibble::tibble(Trait = tbl_search_misc$Trait[idx], MeanValue = n_cur_misc)
    if (is.null(tbl_result_misc)){
      tbl_result_misc <- tbl_cur_result_misc
    } else {
      tbl_result_misc <- dplyr::bind_rows(tbl_result_misc, tbl_cur_result_misc)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_prop, tbl_result_misc)


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for birth and weaning weight
  if(production_system == 1){
    ps_end_statement2extract_wt = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_input13,]
  }else{
    ps_end_statement2extract_wt = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_input08,]
  }

  vec_ecow_result_birth_wean <- extract_result(ps_path_2outputfile,
                                               ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_input06,],
                                               ps_end_statement2extract = ps_end_statement2extract_wt,
                                               pb_log,
                                               plogger = lgr)
  tbl_result_birth_wean <- NULL
  tbl_search_birth_wean <- tbl_search[l_constants_postprocess_beefOnbeef$search_birth_wean,]
  for (idx in 1:nrow(tbl_search_birth_wean)){
    n_cur_birth_wean <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_birth_wean,
                                   ps_statement2search = tbl_search_birth_wean$SearchPattern[idx],
                                   ps_line2get = tbl_search_birth_wean$IndexOffset[idx],
                                   ps_splitby = ":",
                                   pb_log,
                                   plogger = lgr)
    n_cur_birth_wean <- as.numeric(n_cur_birth_wean[l_constants_postprocess_beefOnbeef$string_2])
    tbl_cur_result_birth_wean <- tibble::tibble(Trait = tbl_search_birth_wean$Trait[idx], MeanValue = n_cur_birth_wean)
    if (is.null(tbl_result_birth_wean)){
      tbl_result_birth_wean <- tbl_cur_result_birth_wean
    } else {
      tbl_result_birth_wean <- dplyr::bind_rows(tbl_result_birth_wean, tbl_cur_result_birth_wean)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_birth_wean)


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for carcass weight
  if(production_system == 1){
    ps_end_statement2extract_carcass = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_input05,]
  }else{
    ps_end_statement2extract_carcass = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_input04,]
  }

  vec_ecow_result_slaughter <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOnbeef$idx_row_input08,],
                                              ps_end_statement2extract = ps_end_statement2extract_carcass,
                                              pb_log,
                                              plogger = lgr)
  tbl_result_slaughter <- NULL
  tbl_search_slaughter <- tbl_search[l_constants_postprocess_beefOnbeef$search_slaughter,]
  for (idx in 1:nrow(tbl_search_slaughter)){
    n_cur_slaughter <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_slaughter,
                                         ps_statement2search = tbl_search_slaughter$SearchPattern[idx],
                                         ps_line2get = tbl_search_slaughter$IndexOffset[idx],
                                         ps_splitby = ":",
                                         pb_log,
                                         plogger = lgr)
    n_cur_slaughter <- as.numeric(n_cur_slaughter[l_constants_postprocess_beefOnbeef$string_2])
    tbl_cur_result_slaughter <- tibble::tibble(Trait = tbl_search_slaughter$Trait[idx], MeanValue = n_cur_slaughter)
    if (is.null(tbl_result_slaughter)){
      tbl_result_slaughter <- tbl_cur_result_slaughter
    } else {
      tbl_result_slaughter <- dplyr::bind_rows(tbl_result_slaughter, tbl_cur_result_slaughter)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_slaughter)


  ### # Calculation of some average values
  if(production_system == 1) {
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
    calving <- l_constants_postprocess_beefOnbeef$idx_row_avg_calving_1

  } else if(production_system == 3){
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
    calving <- l_constants_postprocess_beefOnbeef$idx_row_avg_calving
  }


  Fleshiness <- ((tbl_result_mean$MeanValue[fleshiness_m]*tbl_result_mean$MeanValue[prop_m]) + (tbl_result_mean$MeanValue[fleshiness_f]*tbl_result_mean$MeanValue[prop_f]))/(tbl_result_mean$MeanValue[prop_m]+tbl_result_mean$MeanValue[prop_f])
  Fat <- ((tbl_result_mean$MeanValue[fat_m]*tbl_result_mean$MeanValue[prop_m]) + (tbl_result_mean$MeanValue[fat_f]*tbl_result_mean$MeanValue[prop_f]))/(tbl_result_mean$MeanValue[prop_m]+tbl_result_mean$MeanValue[prop_f])
  Birth_weight <- (tbl_result_mean$MeanValue[bw_m]+tbl_result_mean$MeanValue[bw_f])/2
  Weaning_weight <- (tbl_result_mean$MeanValue[wean_wt_m]+tbl_result_mean$MeanValue[wean_wt_f])/2

  l_constant <- get_constants()
  AgeAdjusted_Carcass_weight <- (tbl_result_mean$MeanValue[slaughter_m]*l_constant$dressingpercentage_male*tbl_result_mean$MeanValue[prop_m] + tbl_result_mean$MeanValue[slaughter_f]*l_constant$dressingpercentage_female*tbl_result_mean$MeanValue[prop_f])/(tbl_result_mean$MeanValue[prop_m]+tbl_result_mean$MeanValue[prop_f])


  ### # Tranformation for some values economic weight
  # For age corrected carcass weight we need to convert the economic values of average daily gain:
  # we divide the economic weight for ADG by the total number of days of fattening.
  # This gives the EV per gram increase in carcass weight → multiply by 1000 to get EV per kg increase in carcass weight.
  Slaughter_weight_EV <- tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_ADG]/avg_length_fat
  ACCW_EW <- round(Slaughter_weight_EV*l_constant$dressingpercentage_male*1000, digits = 2) #*0.58 to get from slaughter to carcass weight, *1000 to go from g to kg

  tbl_ACCW <- tibble::tibble(Trait = "EWAgeCorrectedCarcassWeight", EconomicValueDirect = ACCW_EW, EconomicValueMaternal = NA)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_ACCW)


  tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                         ps_start_calving_date = s_start_date,
                                         ps_end_calving_date = s_end_date,
                                         pb_log = b_log,
                                         plogger = NULL)

  tbl_input <- tbl_calving %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>%
    dplyr::filter(Mutter_RasseCode == ps_dambreed)

  tbl_input <- tbl_input %>%
    filter(!is.na(Geburtsverlauf))
  tbl_input <- tbl_input %>%
    filter(Geburtsverlauf != 0)
  tbl_input$calving_transform <- NA

  tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% 1] <- l_constants_postprocess_beefOnbeef$calving_t_1
  tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% 2] <- l_constants_postprocess_beefOnbeef$calving_t_2
  tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% c(3, 4)] <- l_constants_postprocess_beefOnbeef$calving_t_3_4

  m_r <- mean(tbl_input$Geburtsverlauf)
  sd_r <- sd(tbl_input$Geburtsverlauf)

  m_t <- mean(tbl_input$calving_transform)
  sd_t <- sd(tbl_input$calving_transform)

  EW_calving_direct <- tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_calving]
  EW_calving_maternal <- tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_calving]

  ew_sd_direct <- as.numeric(EW_calving_direct)*sd_r/l_constants_postprocess_beefOnbeef$calving_t_delta
  ew_sd_t_direct <- as.numeric(EW_calving_direct)*sd_t
  ew_u_direct = -(ew_sd_direct/sd_t)

  ew_sd_maternal <- as.numeric(EW_calving_maternal)*sd_r/l_constants_postprocess_beefOnbeef$calving_t_delta
  ew_sd_t_maternal <- as.numeric(EW_calving_maternal)*sd_t
  ew_u_maternal = -(ew_sd_maternal/sd_t)

  #Add transformed EW for calving score to the EW table
  tbl_transformed_dir <- tibble::tibble(Trait = "EWCalvingPerformanceTransform", EconomicValueDirect = ew_u_direct, EconomicValueMaternal = ew_u_maternal)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_transformed_dir)

  # ****************************************************************************
  ## ---- Combination of Results ----
  # ****************************************************************************
  if(production_system == 1) {
  traits <- c(#"Calving_performance_direct",
              "Calving_performance_direct_transformed",
              #"Calving_performance_maternal",
              "Calving_performance_maternal_transformed",
              "Birth_weight_direct",
              "Birth_weight_maternal",
              "Age_adjusted_carcass_weight",
              "Mean_class_fleshiness",
              "Mean_class_fat",
              "Weaning_weight_direct",
              "Weaning_weight_maternal")
  EW <- c(#round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_calving], digits = 2),
          round(ew_u_direct, digits = 2),
          #round(tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_calving], digits = 2),
          round(ew_u_maternal, digits = 2),
          round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2),
          round(tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2),
          round(ACCW_EW, digits = 2),
          round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fleshiness], digits = 2),
          round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fat], digits = 2),
          round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2),
          round(tbl_result_ew$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2))
  EW_unit = c(#"CHF/0.01 score",
              "CHF/0.01 transformed score",
              #"CHF/0.01 score",
              "CHF/0.01 transformed score",
              "CHF/kg",
              "CHF/kg",
              "CHF/kg",
              "CHF/0.01 score",
              "CHF/0.01 score",
              "CHF/kg",
              "CHF/kg")
  Population_mean <- c(#round(tbl_result_mean$MeanValue[calving], digits = 2),
                       round(m_t, digits = 2),
                       #round(tbl_result_mean$MeanValue[calving], digits = 2),
                       round(m_t, digits = 2),
                       round(Birth_weight, digits = 2),
                       round(Birth_weight, digits = 2),
                       round(AgeAdjusted_Carcass_weight, digits = 2),
                       round(Fleshiness, digits = 2),
                       round(Fat, digits = 2),
                       round(Weaning_weight, digits = 2),
                       round(Weaning_weight, digits = 2))
  }else if (production_system == 3) {
    traits <- c(#"Calving_performance_direct",
                "Calving_performance_direct_transformed",
                #"Calving_performance_maternal",
                "Calving_performance_maternal_transformed",
                "Birth_weight_direct",
                "Birth_weight_maternal",
                "Age_adjusted_carcass_weight",
                "Mean_class_fleshiness",
                "Mean_class_fat",
                "Weaning_weight_direct",
                "Weaning_weight_maternal")
    EW <- c(#round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_calving], digits = 2),
            round(ew_u_direct, digits = 2),
            #"NA",
            "NA",
            round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2),
            "NA",
            round(ACCW_EW, digits = 2),
            round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fleshiness], digits = 2),
            round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fat], digits = 2),
            round(tbl_result_ew$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2),
            "NA")
    EW_unit = c(#"CHF/0.01 score",
                "CHF/0.01 transformed score",
                #"CHF/0.01 score",
                "CHF/0.01 transformed score",
                "CHF/kg",
                "CHF/kg",
                "CHF/kg",
                "CHF/0.01 score",
                "CHF/0.01 score",
                "CHF/kg",
                "CHF/kg")
    Population_mean <- c(#round(tbl_result_mean$MeanValue[calving], digits = 2),
                         round(m_t, digits = 2),
                         #"NA",
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
                                          EW = EW,
                                          EW_unit = EW_unit,
                                          Population_mean = Population_mean)



  name_file <- ps_scenario

  # assign((paste0("df_",name_file)), tbl_aggregate_results, envir=globalenv())

  tbl_aggregate_results
  tbl_aggregate_results$"EW (Population_mean)" <- paste0(tbl_aggregate_results$EW, "   (",tbl_aggregate_results$Population_mean,")")
  tbl_aggregate_results <- dplyr::select(tbl_aggregate_results, Traits, EW_unit, "EW (Population_mean)")
  tbl_aggregate_results <- data.frame(t(tbl_aggregate_results))
  colnames(tbl_aggregate_results) <- traits
  tbl_aggregate_results <- tbl_aggregate_results[-1,]
  rownames(tbl_aggregate_results) <- c("EW_unit", paste0(name_file))
  assign((paste0("df_",name_file)), tbl_aggregate_results, envir=globalenv())
  write.csv(tbl_aggregate_results, file = paste0(ps_path_tbl_save,"/df_",name_file, ".csv"), row.names = TRUE)

  pie_chart_functional <-  plot_piechart_ewbc(ps_path_2genSD = s_input_genetic_SD,
                                              ptbl_EW_results = tbl_result_ew,
                                              ps_traitgroup2consider = "Functional Traits",
                                              ps_scenario = ps_scenario,
                                              ps_prodsystew = production_system,
                                              pb_log = b_log)

  pie_chart_carcass <-  plot_piechart_ewbc(ps_path_2genSD = s_input_genetic_SD,
                                           ptbl_EW_results = tbl_result_ew,
                                           ps_traitgroup2consider = "Carcass Traits",
                                           ps_scenario = ps_scenario,
                                           ps_prodsystew = production_system,
                                           pb_log = b_log)

  pie_chart_combined <-  plot_piechart_ewbc(ps_path_2genSD = s_input_genetic_SD,
                                            ptbl_EW_results = tbl_result_ew,
                                            ps_traitgroup2consider = "Combined",
                                            ps_scenario = ps_scenario,
                                            ps_prodsystew = production_system,
                                            pb_log = b_log)


  opar <- par()
  pdf(file = paste0(ps_path_tbl_save,"/plots_",name_file, ".pdf"), onefile = TRUE, width = 25)
  gridExtra::grid.table(tbl_aggregate_results)
  par(mfrow = c(3,1))
  print(pie_chart_functional)
  print(pie_chart_carcass)
  print(pie_chart_combined)
  par(opar)
  dev.off()

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
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
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
                               ps_prodsystew,
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
  tbl_gen_SD <- read_file_input(ps_path_2genSD,
                                pb_log,
                                plogger = lgr)

  l_constants_postprocess_beefOnbeef <- get_constants_postprocess_beefOnbeef()

  if (ps_prodsystew == 1) {
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
  #calving score are on different rows for beef and export animals:
  if (ps_prodsystew == 1){
    EW_calving_score_mat <- (round(ptbl_EW_results$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_calving_transform], digits = 2))
    EW_birthwt_mat <- (round(ptbl_EW_results$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2))
    EW_Weantwt_mat <- (round(ptbl_EW_results$EconomicValueMaternal[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2))
    EW_calving_score_dir <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_calving_transform], digits = 2))
    EW_birthwt_dir <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2))
    EW_Weantwt_dir <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2))
    EW_ACCW <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_ACCW], digits = 2))*100 #EBV is in unit dt whereas EW is in unit kg
    EW_fleshiness <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fleshiness], digits = 2))*100 #unit 1 score
    EW_fat <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fat], digits = 2))*100 #unit 1 score
  } else {
  EW_calving_score_dir <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_calving_transform_3], digits = 2))
  EW_birthwt_dir <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_birthwt], digits = 2))
  EW_Weantwt_dir <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_weanwt], digits = 2))
  EW_ACCW <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_ACCW_3], digits = 2))*100 #EBV is in unit dt whereas EW is in unit kg
  EW_fleshiness <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fleshiness], digits = 2))*100 #unit 1 score
  EW_fat <- (round(ptbl_EW_results$EconomicValueDirect[l_constants_postprocess_beefOnbeef$ew_fat], digits = 2))*100 #unit 1 score
  }

  ### # Ensure the economic weight is positive using absolute value for calculation of percentages
  ### # multipling economic weight with the genetic standard deviation to compare traits
  if(ps_prodsystew == 1) {
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
  if(ps_prodsystew == 1) {
    #for carcass traits (production system 1 requires maternal and direct components of traits)
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

  }else if(ps_prodsystew == 3) {
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

  sum_carcass <- sum(ACCW, fleshiness, fat) #Need to add gestation length when we have a solution to its calculation
  carcass_wt_perc <- (ACCW/sum_carcass)*100
  carcass_conformation_perc <- (fleshiness/sum_carcass)*100
  carcass_fat_percentage <- (fat/sum_carcass)*100

  ### # Depending on the trait group to consider
  if (ps_prodsystew == 1){
  if(ps_traitgroup2consider == "Carcass Traits"){
    df <- data.frame(trait = c("Age corrected slaughter weight","Carcass conformation", "Carcass fat"),
                     value = c(carcass_wt_perc, carcass_conformation_perc, carcass_fat_percentage))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    df <- data.frame(trait = c("Calving ease direct", "Calving ease maternal", "Birth weight direct", "Birth weight maternal", "Weaning weight direct", "Weaning weight maternal"),
                     value = c(calving_dir_percentage, calving_mat_percentage, birthwt_dir_percentage, birthwt_mat_percentage, weanwt_dir_percentage, weanwt_mat_percentage))
  }else if (ps_traitgroup2consider == "Combined") {
    df <- data.frame(trait = c("Calving ease direct", "Calving ease maternal", "Birth weight direct", "Birth weight maternal", "Weaning weight direct", "Weaning weight maternal", "Age corrected slaughter weight","Carcass conformation", "Carcass fat"),
                     value = c(calving_ease_perc_dir_comb, calving_ease_perc_mat_comb, birth_weight_perc_dir_comb, birth_weight_perc_mat_comb, wean_wt_perc_dir_comb, wean_wt_perc_mat_comb, carcass_weight_percentage_comb, fleshiness_percentage_comb, fat_percentage_comb))
  }
  }

  if (ps_prodsystew == 3){
    if(ps_traitgroup2consider == "Carcass Traits"){
      df <- data.frame(trait = c("Age corrected slaughter weight","Carcass conformation", "Carcass fat"),
                       value = c(carcass_wt_perc, carcass_conformation_perc, carcass_fat_percentage))
    }else if(ps_traitgroup2consider == "Functional Traits"){
      df <- data.frame(trait = c("Calving ease direct", "Birth weight direct", "Weaning weight direct"),
                       value = c(calving_dir_percentage, birthwt_dir_percentage, weanwt_dir_percentage))
    }else if (ps_traitgroup2consider == "Combined") {
      df <- data.frame(trait = c("Calving ease direct", "Birth weight direct", "Weaning weight direct", "Age corrected slaughter weight","Carcass conformation", "Carcass fat"),
                       value = c(calving_ease_perc_dir_comb, birth_weight_perc_dir_comb, wean_wt_perc_dir_comb, carcass_weight_percentage_comb, fleshiness_percentage_comb, fat_percentage_comb))
    }
  }

  ### # Pie chart
  carcass_pie <- ggplot2::ggplot(df, aes(x = "" , y = value, fill = forcats::fct_inorder(trait))) +
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
    piechart <- carcass_pie + scale_fill_manual(values=c("deepskyblue3", "darkolivegreen3", "gold1"))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    if(ps_prodsystew == 1) {
      piechart <- carcass_pie + scale_fill_manual(values=c("darkorchid1","cornflowerblue", "coral1", "chocolate", "aquamarine4", "darkturquoise"))
    }else{
      piechart <- carcass_pie + scale_fill_manual(values=c("darkorchid1", "coral1", "aquamarine4"))
    }
  } else if (ps_traitgroup2consider == "Combined") {
    if(ps_prodsystew == 1) {
      piechart <- carcass_pie + scale_fill_manual(values=c("darkorchid1","cornflowerblue", "coral1", "chocolate", "aquamarine4", "darkturquoise", "deepskyblue3", "darkolivegreen3", "gold1"))
    }else{
    piechart <- carcass_pie + scale_fill_manual(values=c("darkorchid1","coral1", "aquamarine4", "deepskyblue3", "darkolivegreen3", "gold1"))
    }
  }
}


#' @title Post-processing the output-parameter-file of ECOWEIGHT beef on dairy
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' produce output file. This function processed different functions
#' to extract information from the output parameter files.
#'
#' @param ps_path_2outputfile path to output file of ECOWEIGHT
#' @param ps_output_statement output statement in a file
#' @param ps_output_search_pattern output file with the search patterns
#' @param ps_input_genetic_SD input file for genetic standard deviation
#' @param ps_path_tbl_save path tp the directory for saving the results output pdfs
#' @param ps_scenario name of scenario (sire_dam_system_marketingchannel)
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#' @import tibble
#'
#' @export post_process_ewdc_output
post_process_ewdc_output <- function(ps_path_2outputfile,
                                     ps_output_statement,
                                     ps_output_search_pattern,
                                     ps_input_genetic_SD,
                                     ps_path_tbl_save,
                                     ps_scenario,
                                     ps_sirebreed,
                                     ps_dambreed,
                                     pb_log,
                                     plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'post_process_ewdc_output.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'post_process_ewdc_output',
                    paste0('Starting function with parameters:\n * ps_path_2outputfile', ps_path_2outputfile, '\n',
                           ' * ps_output_statement: ', ps_output_statement, '\n',
                           ' * ps_output_search_pattern: ', ps_output_search_pattern, '\n',
                           ' * ps_input_genetic_SD: ', ps_input_genetic_SD, '\n',
                           ' * ps_path_tbl_save: ', ps_path_tbl_save, '\n',
                           ' * ps_scenario: ', ps_scenario, '\n',
                           ' * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n'))
  }


  ### # Read file with output statement to search in ECOWEIGHT output
  tbl_output_statement <- read_file_input(ps_output_statement,
                                          pb_log,
                                          plogger = lgr)


  tbl_search <- read_file_input(ps_input_file = ps_output_search_pattern,
                                pb_log,
                                plogger = lgr)


  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()

#if the scenario name has "Export" in, a 1 will be returned and we know this is the export marketing channel. If integer(0) is returned, it is Beef/Veal etc..
#This is important because the results files from ecoweight differ between export calves vs. conventional/veal production.
#THis is also required to distinguish between the different genetic standard deviation values depending on beef or veal
  if(length(grep(pattern = "Export", ps_scenario, fixed = TRUE)) > 0 ) {
    marketing_channel <- "Export"
  }else if (length(grep(pattern = "ConventionalBeef", ps_scenario, fixed = TRUE)) > 0 ) {
    marketing_channel <- "Beef"
  } else if (length(grep(pattern = "ConventionalVeal", ps_scenario, fixed = TRUE)) > 0 ){
    marketing_channel <- "Veal"
  }
  # ****************************************************************************
  ## ---- Economic weight results ----
  # ****************************************************************************
  ### # Extract the part of interest of the results coming from ECOWEIGHT output
 if(marketing_channel != "Export"){
   vec_ecow_result_EW <- extract_result(ps_path_2outputfile,
                                       ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_marginal_EW,],
                                       ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_direct_maternal_EW,],
                                       pb_log,
                                       plogger = lgr)


  ### # Get the value
  tbl_result_ew <- NULL
  for(idx in 1:nrow(tbl_search[l_constants_postprocess_beefOndairy$ew_results,])){
    n_cur_ew <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_EW,
                                 ps_statement2search = tbl_search$SearchPattern[idx],
                                 ps_line2get = tbl_search$IndexOffset[idx],
                                 ps_splitby = "        ",
                                 pb_log,
                                 plogger = lgr)
    n_cur_ew <- as.numeric(n_cur_ew[l_constants_postprocess_beefOndairy$string_EW_value])
    tbl_cur_ew <- tibble::tibble(Trait = tbl_search$Trait[idx], EconomicValue = n_cur_ew)
    if (is.null(tbl_result_ew)){
      tbl_result_ew <- tbl_cur_ew
    } else {
      tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_cur_ew)
    }
  }


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for fattening length (time from the end of rearing to slaughter) NOT REQUIRED FOR EXPORT CALVES
  ### # Average fattening length is required for calculating the economic weight for age adjusted carcass weight: EW ADG/days fattening
  vec_ecow_result_fat_male <- extract_result(ps_path_2outputfile,
                                        ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_fattened_bull,],
                                        ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_fattened_dairyheifer,],
                                        pb_log,
                                        plogger = lgr)
  tbl_result_ew_fat_m <- NULL
  tbl_search_fat_m <- tbl_search[l_constants_postprocess_beefOndairy$search_fatm,]
  n_cur_ew_fat_m <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat_male,
                                     ps_statement2search = tbl_search_fat_m$SearchPattern,
                                     ps_line2get = tbl_search_fat_m$IndexOffset,
                                     ps_splitby = "    ",
                                     pb_log,
                                     plogger = lgr)
    n_cur_ew_fat_m <- as.numeric(n_cur_ew_fat_m[l_constants_postprocess_beefOndairy$string_4])
    tbl_cur_ew_fat_m <- tibble::tibble(Trait = tbl_search_fat_m$Trait, EconomicValue = n_cur_ew_fat_m)
    if (is.null(tbl_result_ew_fat_m)){
      tbl_result_ew_fat_m <- tbl_cur_ew_fat_m
    } else {
      tbl_result_ew_fat_m <- dplyr::bind_rows(tbl_result_ew_fat_m, tbl_cur_ew_fat_m)
    }

    vec_ecow_result_fat_heifer <- extract_result(ps_path_2outputfile,
                                               ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_fattened_beefheifer,],
                                               ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_heifers,],
                                               pb_log,
                                               plogger = lgr)
    tbl_result_ew_fat_f <- NULL
    tbl_search_fat_f <- tbl_search[l_constants_postprocess_beefOndairy$search_fatf,]
    n_cur_ew_fat_f <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_fat_heifer,
                                     ps_statement2search = tbl_search_fat_f$SearchPattern,
                                     ps_line2get = tbl_search_fat_f$IndexOffset,
                                     ps_splitby = "    ",
                                     pb_log,
                                     plogger = lgr)
    n_cur_ew_fat_f <- as.numeric(n_cur_ew_fat_f[l_constants_postprocess_beefOndairy$string_4])
    tbl_cur_ew_fat_f <- tibble::tibble(Trait = tbl_search_fat_f$Trait, EconomicValue = n_cur_ew_fat_f)
    if (is.null(tbl_result_ew_fat_f)){
      tbl_result_ew_fat_f <- tbl_cur_ew_fat_f
    } else {
      tbl_result_ew_fat_f <- dplyr::bind_rows(tbl_result_ew_fat_f, tbl_cur_ew_fat_f)
    }

    tbl_result_ew_fat <- dplyr::bind_rows(tbl_result_ew_fat_m, tbl_result_ew_fat_f)

  avg_fattening_length <- sum(tbl_result_ew_fat$EconomicValue)/2

  # ### Calculation of Age adjusted carcass weight Economic Weight
  ### # Tranformation for some values economic weight
  # For age corrected carcass weight we need to convert the economic values of average daily gain:
  # we divide the economic weight for ADG by the total number of days of fattening.
  # This gives the EV per gram increase in carcass weight → multiply by 1000 to get EV per kg increase in carcass weight.
  AASW_EW <- (tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_ADG])/(avg_fattening_length)*1000
  ACCW <- AASW_EW*l_constants_postprocess_beefOndairy$avg_dressing
  tbl_ACCW <- tibble::tibble(Trait = "EWAgeCorrectedCarcassWeight", EconomicValue = ACCW)
  tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_ACCW)

  # ****************************************************************************
  ## ---- Average values ----
  # ****************************************************************************

  ### # Extract the part of interest of the results coming from ECOWEIGHT output for calving performance, fleshiness, fat
  vec_ecow_result_misc <- extract_result(ps_path_2outputfile,
                                         ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_miscellaneous,],
                                         ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_nutrition,],
                                         pb_log,
                                         plogger = lgr)
  tbl_result_misc <- NULL
  tbl_search_misc <- tbl_search[l_constants_postprocess_beefOndairy$search_misc,]
  for (idx in 1:nrow(tbl_search_misc)){
    n_cur_misc <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_misc,
                                   ps_statement2search = tbl_search_misc$SearchPattern[idx],
                                   ps_line2get = tbl_search_misc$IndexOffset[idx],
                                   ps_splitby = "     ",
                                   pb_log,
                                   plogger = lgr)
    n_cur_misc <- as.numeric(n_cur_misc[l_constants_postprocess_beefOndairy$string_3])
    tbl_cur_result_misc <- tibble::tibble(Trait = tbl_search_misc$Trait[idx], MeanValue = n_cur_misc)
    if (is.null(tbl_result_misc)){
      tbl_result_misc <- tbl_cur_result_misc
    } else {
      tbl_result_misc <- dplyr::bind_rows(tbl_result_misc, tbl_cur_result_misc)
    }
  }
  tbl_result_mean <- tbl_result_misc


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for birth weight
  vec_ecow_result_birthwt <- extract_result(ps_path_2outputfile,
                                               ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT15,],
                                               ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT11,],
                                               pb_log,
                                               plogger = lgr)
  tbl_result_birthwt<- NULL
  tbl_search_birthwt <- tbl_search[l_constants_postprocess_beefOndairy$search_bw,]
  for (idx in 1:nrow(tbl_search_birthwt)){
    n_cur_birthwt <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_birthwt,
                                         ps_statement2search = tbl_search_birthwt$SearchPattern[idx],
                                         ps_line2get = tbl_search_birthwt$IndexOffset[idx],
                                         ps_splitby = "     ",
                                         pb_log,
                                         plogger = lgr)
    n_cur_birthwt <- as.numeric(n_cur_birthwt[l_constants_postprocess_beefOndairy$string_2])


    tbl_cur_result_birthwt <- tibble::tibble(Trait = tbl_search_birthwt$Trait[idx], MeanValue = n_cur_birthwt)
    if (is.null(tbl_result_birthwt)){
      tbl_result_birthwt <- tbl_cur_result_birthwt
    } else {
      tbl_result_birthwt <- dplyr::bind_rows(tbl_result_birthwt, tbl_cur_result_birthwt)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_birthwt)

  ### # Extract the part of interest of the results coming from ECOWEIGHT output for gestation length
  vec_ecow_result_gestation <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT11,],
                                              ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT23,],
                                              pb_log,
                                              plogger = lgr)

  tbl_result_gestation<- NULL
  tbl_search_gestation <- tbl_search[l_constants_postprocess_beefOndairy$search_gestation,]
  n_cur_gestation <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_gestation,
                                    ps_statement2search = tbl_search_gestation$SearchPattern,
                                    ps_line2get = tbl_search_gestation$IndexOffset,
                                    ps_splitby = "    ",
                                    pb_log,
                                    plogger = lgr)
  n_cur_gestation <- as.numeric(n_cur_gestation[l_constants_postprocess_beefOndairy$string_2])

  tbl_cur_result_gestation <- tibble::tibble(Trait = tbl_search_gestation$Trait, MeanValue = n_cur_gestation)
  if (is.null(tbl_result_gestation)){
    tbl_result_gestation <- tbl_cur_result_gestation
  } else {
    tbl_result_gestation <- dplyr::bind_rows(tbl_result_gestation, tbl_cur_result_gestation)
  }
tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_gestation)


  ### # Extract the part of interest of the results coming from ECOWEIGHT output for carcass weight
  vec_ecow_result_slaughter <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT23,],
                                              ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT22,],
                                              pb_log,
                                              plogger = lgr)
  tbl_result_slaughter <- NULL
  tbl_search_slaughter <- tbl_search[l_constants_postprocess_beefOndairy$search_slaughter,]
  for (idx in 1:nrow(tbl_search_slaughter)){
    n_cur_slaughter <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_slaughter,
                                        ps_statement2search = tbl_search_slaughter$SearchPattern[idx],
                                        ps_line2get = tbl_search_slaughter$IndexOffset[idx],
                                        ps_splitby = "    ",
                                        pb_log,
                                        plogger = lgr)
    n_cur_slaughter <- as.numeric(n_cur_slaughter[l_constants_postprocess_beefOndairy$string_2])
    tbl_cur_result_slaughter <- tibble::tibble(Trait = tbl_search_slaughter$Trait[idx], MeanValue = n_cur_slaughter)
    if (is.null(tbl_result_slaughter)){
      tbl_result_slaughter <- tbl_cur_result_slaughter
    } else {
      tbl_result_slaughter <- dplyr::bind_rows(tbl_result_slaughter, tbl_cur_result_slaughter)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_slaughter)


  ### # Calculation of some average values
  Fleshiness <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_flesh_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_flesh_f]))/2
  Fat <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_fat_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_fat_f]))/2
  Birth_weight <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_f]))/2
  Calving_score <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_f]))/2

  l_constant <- get_constants()


  # ### Calculation of carcass weight avg:: not included in results output so needs to be calculated separately
  # e.g. for heifers: Slaughter wt = (Daily gain of heifers in fattening * days in fattening)+ average weight of female calves at end of the rearing period
  # To make it the average for carcass weight, we then need to multiply by the dressing proportion for heifers and males then average the carcass weight female and male to find one value

  vec_ecow_result_carcasswt <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT15,],
                                              ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT11,],
                                              pb_log,
                                              plogger = lgr)
  tbl_result_carcass <- NULL
  tbl_search_carcass <- tbl_search[l_constants_postprocess_beefOndairy$search_carcass,]
  for (idx in 1:nrow(tbl_search_carcass)){
    n_cur_carcass <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_carcasswt,
                                        ps_statement2search = tbl_search_carcass$SearchPattern[idx],
                                        ps_line2get = tbl_search_carcass$IndexOffset[idx],
                                        ps_splitby = "    ",
                                        pb_log,
                                        plogger = lgr)
    n_cur_carcass <- as.numeric(n_cur_carcass[l_constants_postprocess_beefOndairy$string_2])
    tbl_cur_result_carcass <- tibble::tibble(Trait = tbl_search_carcass$Trait[idx], MeanValue = n_cur_carcass)
    if (is.null(tbl_cur_result_carcass)){
      tbl_result_carcass <- tbl_cur_result_carcass
    } else {
      tbl_result_carcass <- dplyr::bind_rows(tbl_result_carcass, tbl_cur_result_carcass)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_carcass)
# Average daily gain during fattening averaged over male and female
  ADG_fattening <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_ADGm])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_ADGf]))/2
# Average length of fattening
  avg_fattening_length
# Weight at the end of rearing
  vec_ecow_result_rearingwt <- extract_result(ps_path_2outputfile,
                                              ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_growth,],
                                              ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_reproduction,],
                                              pb_log,
                                              plogger = lgr)
  tbl_result_rearing <- NULL
  tbl_search_rearing <- tbl_search[l_constants_postprocess_beefOndairy$search_rearing,]
  for (idx in 1:nrow(tbl_search_rearing)){
    n_cur_rearing <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_rearingwt,
                                      ps_statement2search = tbl_search_rearing$SearchPattern[idx],
                                      ps_line2get = tbl_search_rearing$IndexOffset[idx],
                                      ps_splitby = "    ",
                                      pb_log,
                                      plogger = lgr)
    n_cur_rearing <- as.numeric(n_cur_rearing[l_constants_postprocess_beefOndairy$string_2])
    tbl_cur_result_rearing <- tibble::tibble(Trait = tbl_search_rearing$Trait[idx], MeanValue = n_cur_rearing)
    if (is.null(tbl_cur_result_rearing)){
      tbl_result_rearing <- tbl_cur_result_rearing
    } else {
      tbl_result_rearing <- dplyr::bind_rows(tbl_result_rearing, tbl_cur_result_rearing)
    }
  }
  tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_rearing)

Rearing_wt <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_rearingwt_m])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_row_rearingwt_f]))/2
# Calculate avg slaughter weight
avg_slaughter_wt <- (ADG_fattening*avg_fattening_length) + Rearing_wt
# Convert to average carcass weight by multiplying by average dressing proportion (0.58 for bulls and 0.54 for heifers = 0.56)
avg_carcass_wt <- avg_slaughter_wt*l_constants_postprocess_beefOndairy$avg_dressing
# Add result to mean table
tbl_avg_carcasswt <- tibble::tibble(Trait = "Carcass_wt", MeanValue = avg_carcass_wt)

tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_avg_carcasswt)


#Transformation of calving score economic weight to scale used for EBV
# EW_calving <-tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving]
# ps_EW_calving <- EW_calving

tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                       ps_start_calving_date = s_start_date,
                                       ps_end_calving_date = s_end_date,
                                       pb_log = b_log,
                                       plogger = NULL)

tbl_input <- tbl_calving %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>%
  dplyr::filter(Mutter_RasseCode == ps_dambreed)


tbl_input <- tbl_input %>%
  filter(!is.na(Geburtsverlauf))
tbl_input <- tbl_input %>%
  filter(Geburtsverlauf != 0)
tbl_input$calving_transform <- NA

tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% 1] <- l_constants_postprocess_beefOndairy$calving_t_1
tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% 2] <- l_constants_postprocess_beefOndairy$calving_t_2
tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% c(3, 4)] <- l_constants_postprocess_beefOndairy$calving_t_3_4

m_r <- mean(tbl_input$Geburtsverlauf)
sd_r <- sd(tbl_input$Geburtsverlauf)

m_t <- mean(tbl_input$calving_transform)
sd_t <- sd(tbl_input$calving_transform)

EW_calving <- tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving]

ew_sd <- as.numeric(EW_calving)*sd_r/l_constants_postprocess_beefOndairy$calving_t_delta
ew_sd_t <- as.numeric(EW_calving)*sd_t
ew_u = -(ew_sd/sd_t)

#Add transformed EW for calving score to the EW table
tbl_transformed <- tibble::tibble(Trait = "EWCalvingPerformanceTransform", EconomicValue = ew_u)
tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_transformed)

  # ****************************************************************************
  ## ---- Combination of Results ----
  # ****************************************************************************
 traits <-  c("Calving_performance",
              "Calving_performance_transformed",
              "Birth_weight",
              "Age_adjusted_carcass_weight",
              "Mean_class_fleshiness",
              "Mean_class_fat")

tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                          EW = c(round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving], digits = 2),
                                                 round(ew_u, digits = 2),
                                                              round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_birthwt], digits = 2),
                                                              round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_ACCW], digits = 2),
                                                              round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_fleshiness], digits = 2),
                                                              round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_fat], digits = 2)),
                                          EW_unit = c("CHF/0.01 score",
                                                      "CHF/0.01 transformed score",
                                                                   "CHF/kg",
                                                                   "CHF/kg",
                                                                   "CHF/0.01 score",
                                                                   "CHF/0.01 score"),
                                          Population_mean = c(round(Calving_score, digits = 2),
                                                              round(m_t, digits = 2),
                                                                                  round(Birth_weight, digits = 2),
                                                                                  round(avg_carcass_wt, digits = 2),
                                                                                  round(Fleshiness, digits = 2),
                                                                                  round(Fat, digits = 2)))

name_file <- ps_scenario
# assign((paste0("df_",name_file)), tbl_aggregate_results, envir=globalenv())

tbl_aggregate_results
tbl_aggregate_results$"EW (Population_mean)" <- paste0(tbl_aggregate_results$EW, "   (",tbl_aggregate_results$Population_mean,")")
tbl_aggregate_results <- dplyr::select(tbl_aggregate_results, Traits, EW_unit, "EW (Population_mean)")
tbl_aggregate_results <- data.frame(t(tbl_aggregate_results))
tbl_aggregate_results <- tbl_aggregate_results[-1,]
colnames(tbl_aggregate_results) <- traits
rownames(tbl_aggregate_results) <- c("EW_unit", paste0(name_file))
assign((paste0("df_",name_file)), tbl_aggregate_results, envir=globalenv())
write.csv(tbl_aggregate_results, file = paste0(ps_path_tbl_save,"/df_",name_file, ".csv"), row.names = TRUE)

pie_chart_functional <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                            ptbl_EW_results = tbl_result_ew,
                                            ps_traitgroup2consider = "Functional Traits",
                                            ps_scenario = ps_scenario,
                                            ps_marketchannel = marketing_channel,
                                            pb_log = b_log)

pie_chart_carcass <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                         ptbl_EW_results = tbl_result_ew,
                                         ps_traitgroup2consider = "Carcass Traits",
                                         ps_scenario = ps_scenario,
                                         ps_marketchannel = marketing_channel,
                                         pb_log = b_log)

pie_chart_combined <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                          ptbl_EW_results = tbl_result_ew,
                                          ps_traitgroup2consider = "Combined",
                                          ps_scenario = ps_scenario,
                                          ps_marketchannel = marketing_channel,
                                          pb_log = b_log)

#save table and pie charts to pdf
#save original values of parameters
opar <- par()
pdf(file = paste0(ps_path_tbl_save,"/plots_",name_file, ".pdf"), onefile = TRUE, width = 16)
gridExtra::grid.table(tbl_aggregate_results)
par(mfrow = c(3,1))
print(pie_chart_functional)
print(pie_chart_carcass)
print(pie_chart_combined)
par(opar)
dev.off()

 }else if (marketing_channel == "Export"){
   # Table of economic weights for export calves: only calving traits
   vec_ecow_result_EW <- extract_result(ps_path_2outputfile,
                                        ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_marginal_EW,],
                                        ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_direct_maternal_EW,],
                                        pb_log,
                                        plogger = lgr)


   ### # Get the value
   tbl_result_ew <- NULL
   for(idx in 1:nrow(tbl_search[l_constants_postprocess_beefOndairy$ew_results,])){
     n_cur_ew <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_EW,
                                  ps_statement2search = tbl_search$SearchPattern[idx],
                                  ps_line2get = tbl_search$IndexOffset[idx],
                                  ps_splitby = "        ",
                                  pb_log,
                                  plogger = lgr)
     n_cur_ew <- as.numeric(n_cur_ew[l_constants_postprocess_beefOndairy$string_EW_value])
     tbl_cur_ew <- tibble::tibble(Trait = tbl_search$Trait[idx], EconomicValue = n_cur_ew)
     if (is.null(tbl_result_ew)){
       tbl_result_ew <- tbl_cur_ew
     } else {
       tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_cur_ew)
     }
   }
 # Average values for calving score and birth weight
   vec_ecow_result_misc <- extract_result(ps_path_2outputfile,
                                          ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_miscellaneous,],
                                          ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_nutrition,],
                                          pb_log,
                                          plogger = lgr)
   tbl_result_misc <- NULL
   tbl_search_misc <- tbl_search[l_constants_postprocess_beefOndairy$search_misc_export,]
   for (idx in 1:nrow(tbl_search_misc)){
     n_cur_misc <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_misc,
                                    ps_statement2search = tbl_search_misc$SearchPattern[idx],
                                    ps_line2get = tbl_search_misc$IndexOffset[idx],
                                    ps_splitby = "     ",
                                    pb_log,
                                    plogger = lgr)
     n_cur_misc <- as.numeric(n_cur_misc[l_constants_postprocess_beefOndairy$string_3])
     tbl_cur_result_misc <- tibble::tibble(Trait = tbl_search_misc$Trait[idx], MeanValue = n_cur_misc)
     if (is.null(tbl_result_misc)){
       tbl_result_misc <- tbl_cur_result_misc
     } else {
       tbl_result_misc <- dplyr::bind_rows(tbl_result_misc, tbl_cur_result_misc)
     }
   }
   tbl_result_mean <- tbl_result_misc

   #Birth weight avg
   vec_ecow_result_birthwt <- extract_result(ps_path_2outputfile,
                                             ps_start_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT15,],
                                             ps_end_statement2extract = tbl_output_statement[l_constants_postprocess_beefOndairy$idx_row_INPUT11,],
                                             pb_log,
                                             plogger = lgr)
   tbl_result_birthwt<- NULL
   tbl_search_birthwt <- tbl_search[l_constants_postprocess_beefOndairy$search_bw,]
   for (idx in 1:nrow(tbl_search_birthwt)){
     n_cur_birthwt <- get_result_value(pvec_ecow_result_2extract = vec_ecow_result_birthwt,
                                       ps_statement2search = tbl_search_birthwt$SearchPattern[idx],
                                       ps_line2get = tbl_search_birthwt$IndexOffset[idx],
                                       ps_splitby = "     ",
                                       pb_log,
                                       plogger = lgr)
     n_cur_birthwt <- as.numeric(n_cur_birthwt[l_constants_postprocess_beefOndairy$string_3])


     tbl_cur_result_birthwt <- tibble::tibble(Trait = tbl_search_birthwt$Trait[idx], MeanValue = n_cur_birthwt)
     if (is.null(tbl_result_birthwt)){
       tbl_result_birthwt <- tbl_cur_result_birthwt
     } else {
       tbl_result_birthwt <- dplyr::bind_rows(tbl_result_birthwt, tbl_cur_result_birthwt)
     }
   }
   tbl_result_mean <- dplyr::bind_rows(tbl_result_mean, tbl_result_birthwt)


   Birth_weight <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_exportm])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_birthwt_exportf]))/2
   Calving_score <- ((tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_exportm])+(tbl_result_mean$MeanValue[l_constants_postprocess_beefOndairy$idx_calving_exportf]))/2

   #Transform the EW for calving performance
   tbl_calving <- read_file_input_calving(ps_input_file_calving = s_input_file_calving,
                                          ps_start_calving_date = s_start_date,
                                          ps_end_calving_date = s_end_date,
                                          pb_log = b_log,
                                          plogger = NULL)

   tbl_input <- tbl_calving %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>%
     dplyr::filter(Mutter_RasseCode == ps_dambreed)

   tbl_input$calving_transform <- 0
   tbl_input <- tbl_input %>%
     filter(!is.na(Geburtsverlauf))
   tbl_input <- tbl_input %>%
     filter(Geburtsverlauf != 0)

   tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% 1] <- l_constants_postprocess_beefOndairy$calving_t_1
   tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% 2] <- l_constants_postprocess_beefOndairy$calving_t_2
   tbl_input$calving_transform[tbl_input$Geburtsverlauf %in% c(3, 4)] <- l_constants_postprocess_beefOndairy$calving_t_3_4

   calving_score <- tbl_input$Geburtsverlauf
   m_r <- mean(calving_score)
   sd_r <- sd(calving_score)

   calving_transformed <- tbl_input$calving_transform
   m_t <- mean(calving_transformed)
   sd_t <- sd(calving_transformed)

   EW_calving <- tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving]

   ew_sd <- as.numeric(EW_calving)*sd_r/l_constants_postprocess_beefOndairy$calving_t_delta
   ew_sd_t <- as.numeric(EW_calving)*sd_t
   ew_u = -(ew_sd/sd_t)

   #add the transformed EW for calving score to the table of economic weights
   tbl_transformed <- tibble::tibble(Trait = "EWCalvingPerformanceTransform", EconomicValue = ew_u)
   tbl_result_ew <- dplyr::bind_rows(tbl_result_ew, tbl_transformed)

   #Form a table and export as pdf
   traits <-  c("Calving_performance",
                "Calving_performance_transformed",
                "Birth_weight",
                "Age_adjusted_carcass_weight",
                "Mean_class_fleshiness",
                "Mean_class_fat")
   tbl_aggregate_results <- tibble::tibble(Traits =  traits,
                                           EW = c((round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving], digits = 2)),
                                                  round(ew_u, digits = 2),
                                                               round(tbl_result_ew$EconomicValue[l_constants_postprocess_beefOndairy$ew_birthwt], digits = 2), NA, NA, NA),
                                           EW_unit = c("CHF/0.01 score",
                                                       "CHF/0.01 transformed score",
                                                       "CHF/kg",
                                                       "CHF/kg",
                                                       "CHF/0.01 score",
                                                       "CHF/0.01 score"),
                                           Population_mean = c(round(Calving_score, digits = 2),
                                                               round(m_t, digits = 2),
                                                               round(Birth_weight,digits = 2),
                                                               NA,
                                                               NA,
                                                               NA))


   name_file <- ps_scenario
   # assign((paste0("df_",name_file)), tbl_aggregate_results, envir=globalenv())

   tbl_aggregate_results
   tbl_aggregate_results$"EW (Population_mean)" <- paste0(tbl_aggregate_results$EW, "   (",tbl_aggregate_results$Population_mean,")")
   tbl_aggregate_results <- dplyr::select(tbl_aggregate_results, Traits, EW_unit, "EW (Population_mean)")
   tbl_aggregate_results <- data.frame(t(tbl_aggregate_results))
   tbl_aggregate_results <- tbl_aggregate_results[-1,]
   colnames(tbl_aggregate_results) <- traits
   rownames(tbl_aggregate_results) <- c("EW_unit", paste0(name_file))
   assign((paste0("df_",name_file)), tbl_aggregate_results, envir=globalenv())
   write.csv(tbl_aggregate_results, file = paste0(ps_path_tbl_save,"/df_",name_file, ".csv"), row.names = TRUE)

   pie_chart_functional <-  plot_piechart_ewdc(ps_path_2genSD = ps_input_genetic_SD,
                                               ptbl_EW_results = tbl_result_ew,
                                               ps_traitgroup2consider = "Functional Traits",
                                               ps_scenario = ps_scenario,
                                               ps_marketchannel = marketing_channel,
                                               pb_log = b_log)

   opar <- par()
   pdf(file = paste0(ps_path_tbl_save,"/plots_",name_file, ".pdf"), onefile = TRUE, width = 16)
   gridExtra::grid.table(tbl_aggregate_results)
   par(mfrow = c(3,1))
   print(pie_chart_functional)
   par(opar)
   dev.off()

 }
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

  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()

  temp = list.files(path = ps_path_results_tbl, pattern="df_")
  results_tables <- data.frame(temp)

  myfiles = lapply(paste0(ps_path_results_tbl, "/", temp), read.table, sep=",")

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

  sire_breeds <- unique(tbl_info$Sire)
  dam_breeds <- unique(tbl_info$Dam)
  marketing_channels <- unique(tbl_info$Marketing)

  # ps_sort_by <- "dam_breed"

  if(ps_sort_by == "sire_breed") {
    sex_breed <- unique(tbl_info$Sire)
    sex <- "sire"
  }else if (ps_sort_by == "dam_breed") {
    sex_breed <- unique(tbl_info$Dam)
    sex <- "dam"
  } else if (ps_sort_by == "marketing_channel") {
    sex_breed <- unique(tbl_info$Marketing)
    sex <- "marketing"
  }

  for(idx in 1:length(sex_breed)){
    breed <- sex_breed[idx]
    tbl_sex_breed <- myfiles[grep(breed, myfiles)]
    tbl_breed <- NULL

    for (jdx in 1:length(tbl_sex_breed)){
      tbl_current_breed <- as.data.frame(tbl_sex_breed[jdx])
      colnames(tbl_current_breed) <- tbl_current_breed[1,]
      rownames(tbl_current_breed) <- tbl_current_breed[,1]
      tbl_current_breed <- tbl_current_breed[-1,]
      tbl_current_breed <- tbl_current_breed[,-1]
      #
      if(is.null(tbl_breed)){
        tbl_breed <- tbl_current_breed
      } else {
        tbl_breed <- dplyr::bind_rows(tbl_breed, tbl_current_breed[2,])
      }
    }


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


    tbl_breed <- tbl_breed %>%
      dplyr::relocate("SirexDam") %>%
      dplyr::relocate("Marketing_channel")

    tbl_breed[is.na(tbl_breed)] <- "-"
    tbl_breed <- tbl_breed[order((tbl_breed$Marketing_channel)), ]
    row.names(tbl_breed) <- NULL

    pdf(paste0(ps_path_save,"/results_tbl_",sex,"_",breed,".pdf"), height=11, width=20)
    gridExtra::grid.table(tbl_breed)
    dev.off()
  }

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
                           ' * ptbl_aggregate_results \n',
                           ' * ps_traitgroup2consider: ', ps_traitgroup2consider, '\n',
                           ' * ps_scenario: ', ps_scenario, '\n'))
  }


  ### # Read file with genetic standard deviation
  tbl_gen_SD <- read_file_input(ps_path_2genSD,
                                      pb_log,
                                      plogger = lgr)

  l_constants_postprocess_beefOndairy <- get_constants_postprocess_beefOndairy()

  if (ps_marketchannel == "Beef") {
    genetic_SD_ACCW <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_ACCW_adult]
    genetic_SD_fleshiness <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fleshiness_adult]
    genetic_SD_fat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fat_adult]
  } else if (ps_marketchannel == "Veal") {
    genetic_SD_ACCW <- (tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_ACCW_calf])
    genetic_SD_fleshiness <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fleshiness_calf]
    genetic_SD_fat <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_fat_calf]
  }

  genetic_SD_calving_score <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_row_calving_ease]
  gemetic_SD_birth_wt <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_birth_weight]
  genetic_SD_gestation <- tbl_gen_SD$genetic_standarddeviation[l_constants_postprocess_beefOndairy$idx_gestation_length]



  ### # Take economic weights from table and transform them to same unit as EBV
  #calving score are on different rows for beef and export animals:
if (ps_marketchannel == "Export"){
  EW_calving_score <- (round(ptbl_EW_results$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving_transform_export], digits = 2))#using transformed EW we dont need to change the unit
}else{
  EW_calving_score <- (round(ptbl_EW_results$EconomicValue[l_constants_postprocess_beefOndairy$ew_calving_transform], digits = 2)) #using transformed EW we dont need to change the unit
}

  EW_birth_wt <- round(ptbl_EW_results$EconomicValue[l_constants_postprocess_beefOndairy$ew_birthwt], digits = 2)

  if(ps_marketchannel != "Export") {
  EW_fleshiness <- (round(ptbl_EW_results$EconomicValue[l_constants_postprocess_beefOndairy$ew_fleshiness], digits = 2))*100
  EW_fat <- (round(ptbl_EW_results$EconomicValue[l_constants_postprocess_beefOndairy$ew_fat], digits = 2))*100
  EW_ACCW <-(round(ptbl_EW_results$EconomicValue[l_constants_postprocess_beefOndairy$ew_ACCW], digits = 2))*100 #need to convert kg to dt to match EBV
}

  ### # Ensure the economic weight is positive using absolute value for calculation of percentages
  ### # multipling economic weight with the genetic standard deviation to compare traits
  if(ps_marketchannel != "Export") {
  fleshiness <- abs(EW_fleshiness*genetic_SD_fleshiness)
  fat <- abs(EW_fat*genetic_SD_fat)
  carcass_weight <- abs(genetic_SD_ACCW*EW_ACCW)
  }
  calving_ease <- abs(genetic_SD_calving_score*EW_calving_score)
  birth_weight <- abs(gemetic_SD_birth_wt*EW_birth_wt)


  ### # Transform in percentage
  if(ps_marketchannel != "Export") {
    #for carcass traits (not required for export)
  sum_carcass <- sum(fleshiness, fat, carcass_weight)
  fleshiness_percentage <- (fleshiness/sum_carcass)*100
  fat_percentage <- (fat/sum_carcass)*100
  carcass_weight_percentage <- (carcass_weight/sum_carcass)*100
  # for combining functional and carcass traits - not applicable to export calves
  sum_combined <- sum(calving_ease, birth_weight, fleshiness, fat, carcass_weight)
  calving_ease_perc_comb <- (calving_ease/sum_combined)*100
  birth_weight_perc_comb <- (birth_weight/sum_combined)*100
  fleshiness_percentage_comb <- (fleshiness/sum_combined)*100
  fat_percentage_comb <- (fat/sum_combined)*100
  carcass_weight_percentage_comb <- (carcass_weight/sum_combined)*100
  }

  sum_functional <- sum(calving_ease, birth_weight) #Need to add gestation length when we have a solution to its calculation
  calving_ease_perc <- (calving_ease/sum_functional)*100
  birth_weight_perc <- (birth_weight/sum_functional)*100

  ### # Depending on the trait group to consider
  if(ps_traitgroup2consider == "Carcass Traits"){
    df <- data.frame(trait = c("Age corrected slaughter weight","Carcass conformation", "Carcass fat"),
                     value = c(carcass_weight_percentage, fleshiness_percentage, fat_percentage))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    df <- data.frame(trait = c("Calving ease", "Birth weight"),
                     value = c(calving_ease_perc, birth_weight_perc))
  }else if (ps_traitgroup2consider == "Combined") {
    df <- data.frame(trait = c("Calving ease", "Birth weight", "Age corrected slaughter weight","Carcass conformation", "Carcass fat"),
                     value = c(calving_ease_perc_comb, birth_weight_perc_comb, carcass_weight_percentage_comb, fleshiness_percentage_comb, fat_percentage_comb))
  }

  ### # Pie chart
  carcass_pie <- ggplot2::ggplot(df, aes(x = "" , y = value, fill = forcats::fct_inorder(trait))) +
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
    piechart <- carcass_pie + scale_fill_manual(values=c("deepskyblue3", "darkolivegreen3", "gold1"))
  }else if(ps_traitgroup2consider == "Functional Traits"){
    piechart <- carcass_pie + scale_fill_manual(values=c("darkorchid1","coral1"))
  } else if (ps_traitgroup2consider == "Combined") {
    piechart <- carcass_pie + scale_fill_manual(values=c("darkorchid1","coral1", "deepskyblue3", "darkolivegreen3", "gold1"))
  }




  return(piechart)

}


