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
#' Return a list with specific constants that are used.
#'
#' @export get_constants
get_constants <- function(){
  # return list of default constants
  return(list(vec_dressing_female = 0.56,
              vec_dressing_male = 0.58,
              vec_Natura_Beef = 2,
              vec_SwissPrimBeef = 3))
}
