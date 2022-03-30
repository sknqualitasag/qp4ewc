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
  tbl_input <- ps_input_calving_tibble %>% filter(Geburtsverlauf != 0)
  qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                  paste0('Only consider known calving score to calculate abortion rate'))


  ### # Different calculation depending on ps_statement_firstlactation
  if(ps_statement_firstlactation){
    tbl_abort <- tbl_input %>% filter(Laktationsnummer_Mutter == 1) %>%
                                select(Abort) %>%
                                na.omit() %>%
                                group_by(Abort) %>%
                                count()

    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('A Tibble for primiparous has been created for the calculation of abortion rate '))

  }else{
    tbl_abort <- tbl_input %>% filter(Laktationsnummer_Mutter > 1) %>%
      select(Abort) %>%
      na.omit() %>%
      group_by(Abort) %>%
      count()

    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('A Tibble for multiparous has been created for the calculation of abortion rate '))
  }


  ### # The value in case of an abort in the data is 1
  ### # According to the documentation for calving data under https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/1915289939/ZWS+Export+Geburtsablauf+GA
  ### # Check if data for abort are available to calculate abortion rate
  if(nrow(tbl_abort %>% filter(Abort == 1)) != 0){
    qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                    paste0('Abort information are available in the dataset so that abortion rate can be calculated'))
  }else{
    stop("calculate_abortion_rate: no abort information are available in the dataset, please check the dataset !")
  }


  ### # Add frequence according to abort in a vector
  abort_freq <- tbl_abort %>% filter(Abort == 1) %>% pull(n)
  sum_abort_freq <- sum(tbl_abort$n)


  ### # Calculate abortion rate
  abortion_rate <- round(abort_freq/sum_abort_freq,4)
  qp4ewc_log_info(lgr, 'calculate_abortion_rate',
                  paste0('abortion_rate is : ',abortion_rate))



}
