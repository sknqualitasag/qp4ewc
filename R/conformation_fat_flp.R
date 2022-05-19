### #
### #
### #
### #   Purpose:   Function related to the flp input file based on conformation and fat scores
### #   started:   2022-04-19 (skn)
### #
### # ##################################################################### ###


#' @title Build carcass conformation and fat frequencies depending on slaughtercategory
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function will prepare carcass conformation and fat matrices
#' depending on slaughtercategory.
#'
#' @param ps_input_flp_tibble input flp tibble coming from read_file_input_flp in this package
#' @param ps_sex statement of sex ("F" for female and "M" for male)
#' @param ps_marketing_channel statement of marketing-channel for Natura-Beef (==2), SwissPrimBeef(==3)
#' @param ps_flag_cow flag to select for cows or not (TRUE or FALSE)
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#'
#' @return result_matrix matrix with frequencies for carcass conformation and fat
#'
#' @export build_freq_conf_fat
build_freq_conf_fat <- function(ps_input_flp_tibble,
                                ps_sex,
                                ps_marketing_channel = NULL,
                                ps_flag_cow,
                                pb_log = FALSE,
                                plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'build_freq_conf_fat.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                    paste0('Starting function with parameters:\n * ps_input_flp_tibble \n',
                           ' * ps_sex: ', ps_sex,'\n',
                           ' * ps_marketing_channel: ',ps_marketing_channel,'\n',
                           ' * ps_flag_cow: ',ps_flag_cow))
  }

  ### # Setting the ps_marketing_channel in world to the numeric variable s_marketing_channel
  if(!is.null(ps_marketing_channel)){
    if(ps_marketing_channel == "Natura-Beef"){
      s_marketing_channel <- 2
    }else if(ps_marketing_channel == "SwissPrimBeef"){
      s_marketing_channel <- 3
    }else{
      s_marketing_channel <- NULL
    }
  }else{
    s_marketing_channel <- NULL
  }



  ### # Different tibble depending on ps_sex, ps_marketing_channel
  if(ps_sex == "F"){
    if(ps_flag_cow){
      ### # Slaughtercategory for female to consider is VK == 7
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
                   dplyr::filter(`Schlacht-/Masttierkategorie` == 7) %>%
                   dplyr::select(`Fleischigkeit (1. Teil Handelsklasse CHTAX)`,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
                   na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for cow has been created for build frequencies of conformation and fat'))
      }
    }else{
      ### # Slaughtercategory for female to consider is RG == 5
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "F") %>%
                   dplyr::filter(`Schlacht-/Masttierkategorie` == 5) %>%
                   dplyr::filter(Markenprogramm == s_marketing_channel) %>%
                   dplyr::select(`Fleischigkeit (1. Teil Handelsklasse CHTAX)`,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
                   na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for female has been created for build frequencies of conformation and fat'))
      }
    }
  }else{
    ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
    tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == "M") %>%
                 dplyr::filter(`Schlacht-/Masttierkategorie` == 2 | `Schlacht-/Masttierkategorie` == 3) %>%
                 dplyr::filter(Markenprogramm == s_marketing_channel) %>%
                 dplyr::select(`Fleischigkeit (1. Teil Handelsklasse CHTAX)`,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
                 na.omit()
    if(pb_log){
      qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                      paste0('A Tibble for male has been created for build frequencies of conformation and fat'))
    }
  }


  ### # Build a matrix
  mat_input <- as.matrix(table(tbl_input))
  if(pb_log){
    qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                    paste0('A matrix has been created based on the count for conformationXfat'))
  }



  ### # Frequency matrix
  total_input <- sum(mat_input)
  ### # According to the file description https://qualitasag.atlassian.net/wiki/spaces/PROZESS/pages/390398118/ZWS+Export+Fleischleistungspr+fung+FLP
  ### # Column Fleischigkeit (1. Teil Handesklasse CHTAX) in ps_input_flp_tibble
  ### # The content of column Fleischigkeit can be
  ### # empty, 1 = unknown, 2 = C, 3 = H, 4 = T+, 5 = T, 6 = T-, 7 = A, 8 = 1X, 9 = 2X, 10 = 3X
  freq_input <- (mat_input/total_input)*100
  if(pb_log){
    qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                    paste0('A matrix has been updated based on the frequencies for conformationXfat'))
  }

  ### # vec_rownames content the carcass conformation occuring in the ps_input_flp_tibble
  vec_rownames <- row.names(freq_input)
  vec_colnames <- colnames(freq_input)


  ### # These frequencies are set in a 0-matrix. This step reorder the content starting with 1.
  ### # With this step the rownames are not any more corresponding to the content of the column Fleischigkeit in ps_input_flp_tibble.
  ### # Build a 0-matrix with 9 rows (for carcass conformation) and 5 columns (for carcass fat)
  result_matrix <- matrix(0,nrow = 9, ncol = 5)
  ### # Update the result_matrix with freq_input. If a class is missing, the default value is 0.
  for(i in 1:nrow(freq_input)){
    if(as.character(i+1) == vec_rownames[i]){
      for(j in 1:ncol(freq_input)){
        if(as.character(j) == vec_colnames[j]){
          result_matrix[i,j] <- freq_input[i,j]
        }
      }
    }
  }


  return(result_matrix)
  if(pb_log){
    qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                    paste0('A matrix with all classes for conformationXfat has been set up'))
  }

}
