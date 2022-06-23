### #
### #
### #
### #   Purpose:   Function related to constants to use
### #   started:   2022-04-06 (skn)
### #
### # ##################################################################### ###


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
              vec_dressing_female = 0.56,
              vec_dressing_male = 0.58,
              Natura_Beef = 2,
              SwissPrimBeef = 3))
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



