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


  ### # Different tibble depending on ps_sex
  ### # Slaughtercategory for female to consider is RG == 5
  if(ps_sex == "F"){
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
                                         dplyr::select(`Geburtsgewicht Nako`,`ageAtSlaughterInDays`) %>%
                                         na.omit()
    qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                    paste0('A Tibble for female has been created for the calculation of mean birthweight '))
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
                                         dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
                                         dplyr::select(`Geburtsgewicht Nako`,`ageAtSlaughterInDays`) %>%
                                         na.omit()
    qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                    paste0('A Tibble for male has been created for the calculation of mean birthweight '))
  }


  ### # Calculate the mean birthweight
  birthwt <- round(as.numeric(dplyr::summarise(tbl_input, mean_birthwt = mean(`Geburtsgewicht Nako`))),4)
  qp4ewc_log_info(lgr, 'calculate_mean_birthweight',
                  paste0('Mean birthweight for ',ps_sex,' is : ',birthwt))

  return(birthwt)


}
