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
              age_atslaughter_oldercow = 1460,
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
              matingtype_NM = 2))
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
              maturitytype_early = 1,
              maturitytype_medium = 2,
              maturitytype_late = 3))
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
              idx_row_cowwt_beef_3calving = 2
              ))
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
              purebred_dairy = "PurebredDairy",
              purebred_beef = "PurebredBeef",
              crossbred = "Crossbred"))
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
              idx_row_cow_freq_class = 4,
              idx_row_bull_freq_class = 1,
              idx_row_heifer_freq_class = 2,
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

