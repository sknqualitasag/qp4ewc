### #
### #
### #
### #   Purpose:   Function related to constants to use
### #   started:   2022-04-06 (skn)
### #
### # ##################################################################### ###


#' @title Get constants
#'
#' @description
#' Return a list with constants that are used.
#'
#' @export get_constants
get_constants <- function(){
  # return list of default constants
  return(list(beefOndairy = 4,
              lactnumb1 = 1,
              lactnumb2 = 2,
              lactnumb3 = 3,
              lactnumb4 = 4,
              abort_value = 1,
              calf_score_nohelp = 1,
              calf_score_slighthelp = 2,
              calf_score_difficult = 3,
              calf_score_cesarean = 4,
              stillbirth_value = 4,
              stillbirth_within24h = 2,
              stillbirth_over24h = 3,
              sex_female = "F",
              sex_male = "M",
              slaughtercategory_RG = 5,
              slaughtercategory_OB = 2,
              slaughtercategory_MT = 3,
              slaughtercategory_VK = 7,
              slaughtercategory_MA = 4,
              slaughtercategory_KV = 1,
              age_atslaughter_olderbull = 1460,
              dressingpercentage_female = 0.56,
              dressingpercentage_male = 0.58,
              wording_NaturaBeef = "Natura-Beef",
              value_NaturaBeef = 2,
              wording_SwissPrimBeef = "SwissPrimBeef",
              value_SwissPrimBeef = 3,
              prodsyst4 = 4,
              wording_conv_fat_beef = "ConventionalBeef",
              wording_conv_fat_calf = "ConventionalVeal"))
}



#' @title Get constants for ewbc input in beef-on-beef production systems
#'
#' @description
#' Return a list with constants in beef-on-beef production systems
#' that are used.
#'
#' @export get_constants_ewbc_input_beefOnbeef
get_constants_ewbc_input_beefOnbeef <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              file_av_price_breeding_bull = "INPUT04.TXT",
              av_price_breeding_bull= "Average price per breeding bull purchased for natural mating",
              av_price_breeding_bull_AN = 5333,
              av_price_breeding_bull_AUCHOB = 4750,
              av_price_breeding_bull_LM = 6059,
              av_price_breeding_bull_SI = 5256,
              file_par = "PARA.TXT",
              prodsyst = "Production System",
              prodsyst1 = 1,
              prodsyst2 = 2,
              prodsyst3 = 3,
              maturitytype = "Maturity type of progeny",
              maturitytype_early = 1,
              maturitytype_medium = 2,
              maturitytype_late = 3,
              matingtype_heifer = "Mating type for heifers",
              matingtype_cow = "Mating type for cows",
              matingtype_AI = 1,
              matingtype_NM = 2,
              genStd = "Genetic standard deviations of the traits",
              genStd_known_d_m = 2,
              genStd_nodiff_d_m = 3,
              file_input03 = "INPUT03.TXT",
              price_semenAI = "Price per portion of semen for AI (including service)",
              price_reinsem = "Price per re-insemination (for semen portion and service)",
              price_semenAI_1 = 80,
              price_semenAI_3 = 50,
              fraction_perftestedcows = "Fraction of performance-tested cows",
              fraction_perftestedcows_1 = 1,
              fraction_perftestedcows_3 = 0,
              file_input19 = "INPUT19.TXT",
              peak_milk = "Peak milk in kg per day",
              peak_milk_1 = 8,
              peak_milk_3 = 12,
              prop_female_fattening = "Proportion of surplus female calves for fattening",
              prop_female_fattening_1 = 0.9,
              prop_female_fattening_3 = 1,
              file_input13 = "INPUT13.TXT",
              pregnant_heifer_sold = "Pregnant heifers sold expressed as proportion of surplus female calves",
              pregnant_heifer_sold_1 = 0.1,
              pregnant_heifer_sold_3 = 0))
}


#' @title Get constants for ew input in beef-on-dairy production systems
#'
#' @description
#' Return a list with constants in beef-on-dairy production systems
#' that are used.
#'
#' @export get_constants_ew_input_beefOndairy
get_constants_ew_input_beefOndairy <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              file_par = "PARAD.TXT",
              prodsyst = "Production System",
              prodsyst4 = 4,
              maturitytype_purebred = "Maturity type of pure-bred progeny",
              maturitytype_crossbred = "Maturity type of cross-bred progeny",
              utilisation_purebred = "Utilization of pure-bred female calves which are not needed for replacement",
              utilisation_crossbred = "Utilization of cross-bred female calves which are not needed for replacement",
              maturitytype_early = 1,
              maturitytype_medium = 2,
              maturitytype_late = 3,
              utilisation_export = 1,
              utilisation_fattening = 2))
}


#' @title Get constants for calving in beef-on-beef production systems
#'
#' @description
#' Return a list with specific calving constants in beef-on-beef production systems
#' that are used.
#'
#' @export get_constants_calving_beefOnbeef
get_constants_calving_beefOnbeef <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              idx_row_abortrate = 1,
              idx_row_stillborn_easy = 2,
              idx_row_stillborn_diff = 3,
              idx_row_class_calving = 4,
              idx_row_class_dystocia = 5,
              idx_row_calvingscore2_female = 6,
              idx_row_calvingscore3_female = 7,
              idx_row_calvingscore4_female = 8,
              idx_row_calvingscore2_male = 9,
              idx_row_calvingscore3_male = 10,
              idx_row_calvingscore4_male = 11,
              idx_row_calfdied48h_dystocia = 12,
              idx_row_calfdied48h_easy = 13,
              idx_row_calfloss = 14,
              idx_row_AI_dystocia = 15,
              idx_row_AI_nodystocia = 16,
              first_element_vector = 1,
              second_element_vector = 2,
              n_class_calving = 4,
              class_dystocia = 3,
              sex_female = "F",
              sex_male = "M",
              calvingscore2 = 2,
              calvingscore3 = 3,
              calvingscore4 = 4))
}

#' @title Get constants for calving in beef-on-dairy production systems
#'
#' @description
#' Return a list with specific calving constants in beef-on-beef production systems
#' that are used.
#'
#' @export get_constants_calving_beefOndairy
get_constants_calving_beefOndairy <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              idx_row_abortrate = 1,
              idx_row_stillborn_easy = 2,
              idx_row_stillborn_diff = 3,
              idx_row_class_calving = 18,
              idx_row_class_dystocia = 19,
              idx_row_calvingscore2_female = 12,
              idx_row_calvingscore3_female = 14,
              idx_row_calvingscore4_female = 16,
              idx_row_calvingscore2_male = 13,
              idx_row_calvingscore3_male = 15,
              idx_row_calvingscore4_male = 17,
              idx_row_calvingscore2_female_pure = 6,
              idx_row_calvingscore3_female_pure = 8,
              idx_row_calvingscore4_female_pure = 10,
              idx_row_calvingscore2_male_pure = 7,
              idx_row_calvingscore3_male_pure = 9,
              idx_row_calvingscore4_male_pure = 11,
              idx_row_calfdied48h_dystocia = 4,
              idx_row_calfdied48h_easy = 5,
              first_element_vector = 1,
              second_element_vector = 2,
              n_class_calving = 4,
              class_dystocia = 3,
              sex_female = "F",
              sex_male = "M",
              calvingscore2 = 2,
              calvingscore3 = 3,
              calvingscore4 = 4))
}

#' @title Get constants for progeny in beef-on-beef production systems
#'
#' @description
#' Return a list with specific constants that are used.
#'
#' @export get_constants_progeny_beefOnbeef
get_constants_progeny_beefOnbeef <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              idx_row_bw_female = 1,
              idx_row_bw_male = 2,
              idx_row_lw_heifer_slaughter = 3,
              idx_row_lw_bull_slaughter = 4,
              idx_row_dg_heifer = 5,
              idx_row_dg_bull = 6,
              idx_row_age_1weighing = 7,
              idx_row_1weighing_female = 8,
              idx_row_1weighing_male = 9,
              idx_row_age_2weighing = 10,
              idx_row_age_3weighing = 11,
              idx_row_2weighing_female = 12,
              idx_row_2weighing_male = 13,
              idx_row_3weighing_female = 14,
              idx_row_3weighing_male = 15,
              idx_row_cowwt_2calving = 16,
              idx_row_cowwt_3calving = 17,
              idx_row_bullwt_mature = 18,
              idx_row_cowwt_1calving = 19,
              idx_row_wtcow_2calving = 20,
              sex_female = "F",
              sex_male = "M",
              extrapol_300d = 300,
              extrapol_302d = 302,
              extrapol_304d = 304,
              extrapol_400d = 400,
              Natura_Beef = 2,
              SwissPrimBeef = 3))
}

#' @title Get constants for progeny in beef-on-dairy production systems
#'
#' @description
#' Return a list with specific constants that are used.
#'
#' @export get_constants_progeny_beefOndairy
get_constants_progeny_beefOndairy <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              sex_female = "F",
              sex_male = "M",
              conv_fattening_beef = "ConventionalBeef",
              conv_fattening_calf = "ConventionalVeal",
              export_calf = "Export",
              purebred_dairy = "PurebredDairy",
              purebred_beef = "PurebredBeef",
              crossbred = "Crossbred",
              idx_row_bw_female = 4,
              idx_row_bw_male = 5,
              idx_row_lw_heifer_slaughter_beef = 21,
              idx_row_lw_bull_slaughter_beef = 20,
              idx_row_lw_heifer_slaughter_dairy = 23,
              idx_row_lw_bull_slaughter_dairy = 24,
              idx_row_cowwt_dairy_3calving = 1,
              idx_row_cowwt_beef_3calving = 2,
              age_1stfeeding = 21,
              rearing_age_update = 90,
              idx_row_rearingage = 7,
              idx_row_age1stfeeding = 6,
              idx_row_rearingdailygain_f = 12,
              idx_row_rearingdailygain_m = 13,
              idx_row_fatdailygain_f = 17,
              idx_row_fatdailygain_m = 18,
              idx_row_agesale_f = 8,
              idx_row_agesale_m = 11,
              idx_row_price_f = 26,
              idx_row_price_m = 27,
              idx_row_utilisation_male = 28,
              male_export = 1,
              male_fattening = 0
              ))
}


#' @title Get constants for carcass in beef-on-beef production systems
#'
#' @description
#' Return a list with specific constants that are used.
#'
#' @export get_constants_carcass_beefOnbeef
get_constants_carcass_beefOnbeef <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              idx_row_cow_freq_class = 1,
              idx_row_bull_freq_class = 2,
              idx_row_heifer_freq_class = 3,
              idx_row_cow_price = 4,
              idx_row_bull_avprice = 5,
              idx_row_bull_price = 6,
              idx_row_heifer_price = 7,
              idx_row_cow_coef = 8,
              idx_row_bull_coef = 9,
              idx_row_heifer_coef = 10,
              idx_row_class_fleshiness = 11,
              idx_row_class_fat = 12,
              sex_female = "F",
              sex_male = "M",
              line1 = 1,
              line2 = 2,
              line3 = 3,
              line4 = 4,
              line5 = 5,
              line6 = 6,
              line7 = 7,
              line8 = 8,
              line9 = 9))
}

#' @title Get constants for carcass in beef-on-beef production systems
#'
#' @description
#' Return a list with specific constants that are used.
#'
#' @export get_constants_carcass_beefOndairy
get_constants_carcass_beefOndairy <- function(){
  # return list of default constants
  return(list(idx_col_input_file = 1,
              idx_col_input = 2,
              idx_col_input_value = 4,
              idx_col_input_value_calf = 5,
              idx_row_bull_freq_class = 9,
              idx_row_heifer_freq_class = 10,
              idx_row_cow_freq_class = 4,
              idx_row_heifer_freq_class_purebred = 2,
              idx_row_bull_freq_class_purebred = 1,
              idx_row_cow_price = 12,
              idx_row_bull_price = 14,
              idx_row_heifer_price = 13,
              idx_row_cow_coef = 8,
              idx_row_bull_coef = 5,
              idx_row_heifer_coef = 6,
              idx_row_class_fleshiness = 16,
              idx_row_class_fat = 17,
              sex_female = "F",
              sex_male = "M",
              line1 = 1,
              line2 = 2,
              line3 = 3,
              line4 = 4,
              line5 = 5,
              line6 = 6,
              line7 = 7,
              line8 = 8,
              line9 = 9))
}

#' @title Get constants for post processing for beef-on_beef
#'
#' @description
#' Return a list with specific constants that are used.
#'
#' @export get_constants_postprocess_beefOnbeef
get_constants_postprocess_beefOnbeef <- function(){
  # return list of default constants
  return(list(idx_row_EW_d_m = 2,
              idx_row_EW_relative = 3,
              EW_values = 1:15,
              string_3 = 3,
              string_2 = 2,
              string_1 = 1,
              idx_row_fattened_bulls = 4,
              idx_row_fattened_heifers = 17,
              idx_row_fattened_heifers1 = 5,
              idx_row_fattened_heifers3 = 16,
              idx_row_fat_length_m = 16,
              idx_row_fat_length_f = 17,
              idx_row_structure = 6,
              idx_row_growth = 7,
              idx_row_prop = 18:19,
              search_ew = 1:14,
              idx_row_misc = 8,
              idx_row_nutrition = 9,
              search_misc = 20:26,
              idx_row_input06 = 10,
              idx_row_input13 = 11,
              idx_row_input08 = 12,
              idx_row_input04 = 13,
              idx_row_input05 = 15,
              idx_row_input10 = 14,
              search_birth_wean = 27:30,
              search_slaughter = 31:32,
              idx_row_avg_prop_m = 1,
              idx_row_avg_prop_f = 2,
              idx_row_avg_fat_m = 3,
              idx_row_avg_fat_f = 4,
              idx_row_avg_flesh_m = 5,
              idx_row_avg_flesh_f = 6,
              idx_row_avg_calving = 7,
              idx_row_avg_bw_m = 8,
              idx_row_avg_bw_f = 9,
              idx_row_avg_wean_m = 10,
              idx_row_avg_wean_f = 11,
              idx_row_avg_slaughter_m = 12,
              idx_row_avg_slaughter_f = 13,
              ew_calving = 1,
              ew_birthwt = 4,
              ew_ADG = 5,
              ew_fleshiness = 7,
              ew_fat = 8,
              ew_weanwt = 9

  ))
}



#' @title Get constants for post processing for beef-on_dairy
#'
#' @description
#' Return a list with specific constants that are used.
#'
#' @export get_constants_postprocess_beefOndairy
get_constants_postprocess_beefOndairy <- function(){
  # return list of default constants
  return(list(idx_row_profit = 1,
              idx_row_marginal_EW = 2,
              idx_row_direct_maternal_EW = 3,
              idx_row_relative_EW = 4,
              idx_row_miscellaneous = 5,
              idx_row_nutrition = 6,
              idx_row_INPUT15 = 7,
              idx_row_INPUT11 = 8,
              idx_row_fattened_bull = 9,
              idx_row_fattened_dairyheifer = 10,
              idx_row_fattened_beefheifer = 11,
              idx_row_heifers = 12,
              idx_row_INPUT23 = 13,
              idx_row_INPUT22 = 14,
              idx_row_fat_m = 1,
              idx_row_fat_f = 2,
              idx_row_flesh_m = 3,
              idx_row_flesh_f = 4,
              idx_calving_m = 6,
              idx_calving_f = 5,
              idx_calving_exportm = 2,
              idx_calving_exportf = 1,
              idx_birthwt_m = 8,
              idx_birthwt_f = 7,
              idx_birthwt_exportm = 4,
              idx_birthwt_exportf = 3,
              idx_row_ADGm = 15,
              idx_row_ADGf = 14,
              idx_row_growth = 15,
              idx_row_reproduction = 16,
              idx_row_rearingwt_f = 17,
              idx_row_rearingwt_m = 16,
              string_EW_value = 2,
              avg_dressing = 0.56,
              ew_results = 1:9,
              search_fatm = 10,
              search_fatf = 11,
              search_misc = 12:17,
              search_misc_export = 16:17,
              search_bw = 18:19,
              search_gestation = 24,
              search_slaughter = 20:23,
              search_carcass = 25:26,
              search_rearing = 27:28,
              string_4 = 4,
              string_3 = 3,
              string_2 = 2,
              ew_ADG = 6,
              ew_ACCW = 10,
              ew_fat = 9,
              ew_fleshiness = 8,
              ew_birthwt = 4,
              ew_calving = 1
              ))
}

