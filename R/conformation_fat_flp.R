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
#' @import dplyr
#'
#' @return result_matrix matrix with frequencies for carcass conformation and fat
#'
#' @export build_freq_conf_fat
build_freq_conf_fat <- function(ps_input_flp_tibble,
                                ps_sex,
                                ps_marketing_channel,
                                ps_flag_cow,
                                pb_log,
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


  ### # Get the constants
  l_constants <- get_constants()

  ### # Different tibble depending on ps_sex, ps_marketing_channel
  if(ps_sex == l_constants$sex_female){
    if(ps_flag_cow){
      ### # Slaughtercategory for female to consider is VK == 7
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_female) %>%
                   dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_VK) %>%
                   dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
                   na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for cow has been created for build frequencies of conformation and fat'))
      }
    }else if(ps_marketing_channel == l_constants$wording_NaturaBeef){
      ### # Slaughtercategory for female to consider is RG == 5
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_female) %>%
                   dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_RG) %>%
                   dplyr::filter(Markenprogramm == l_constants$value_NaturaBeef) %>%
                   dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
                   na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for female beef-on-beef has been created for build frequencies of conformation and fat'))
      }
      }else if(ps_marketing_channel == l_constants$wording_SwissPrimBeef){
        ### # Slaughtercategory for female to consider is RG == 5
        tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_female) %>%
          dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_RG) %>%
          dplyr::filter(Markenprogramm == l_constants$value_SwissPrimBeef) %>%
          dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
          na.omit()
        if(pb_log){
          qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                          paste0('A Tibble for female beef-on-beef has been created for build frequencies of conformation and fat'))
        }
      }else if(ps_marketing_channel == l_constants$wording_conv_fat_calf){
        ### # Slaughtercategory for female to consider is RG == 5
        tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_female) %>%
          dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_KV) %>%
          dplyr::filter(is.na(Markenprogramm)) %>%
          dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
          na.omit()
        if(pb_log){
          qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                          paste0('A Tibble for female beef-on-dairy has been created for build frequencies of conformation and fat'))
        }
    }else{
      ### # Slaughtercategory for female to consider is RG == 5
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_female) %>%
        dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_RG) %>%
        dplyr::filter(is.na(Markenprogramm)) %>%
        dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
        na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for female beef-on-dairy has been created for build frequencies of conformation and fat'))
      }
    }
  }else{
    if(ps_marketing_channel == l_constants$wording_NaturaBeef){
      ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_male) %>%
        dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_OB | `Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_MT) %>%
        dplyr::filter(Markenprogramm == l_constants$value_NaturaBeef) %>%
        dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
        na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for male beef-on-beef has been created for build frequencies of conformation and fat'))
      }
      }else if(ps_marketing_channel == l_constants$wording_SwissPrimBeef){
        ### # Slaughtercategory for male to consider is OB == 2 and MT == 3
        tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_male) %>%
          dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_OB | `Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_MT) %>%
          dplyr::filter(Markenprogramm == l_constants$value_SwissPrimBeef) %>%
          dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
          na.omit()
        if(pb_log){
          qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                          paste0('A Tibble for male beef-on-beef has been created for build frequencies of conformation and fat'))
        }
      }else if(ps_marketing_channel == l_constants$wording_conv_fat_calf){
        ### # Slaughtercategory for female to consider is RG == 5
        tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_male) %>%
          dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_KV) %>%
          dplyr::filter(is.na(Markenprogramm)) %>%
          dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
          na.omit()
        if(pb_log){
          qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                          paste0('A Tibble for female beef-on-dairy has been created for build frequencies of conformation and fat'))
        }
    }else{
      tbl_input <- ps_input_flp_tibble %>% dplyr::filter(`Geschlecht Nako` == l_constants$sex_male) %>%
        dplyr::filter(`Schlacht-/Masttierkategorie` == l_constants$slaughtercategory_MT) %>%
        dplyr::filter(is.na(Markenprogramm)) %>%
        dplyr::select(Fleshiness_transform,`Fettgewebe (2. Teil Handelsklasse CHTAX)`) %>%
        na.omit()
      if(pb_log){
        qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                        paste0('A Tibble for male beef-on-dairy has been created for build frequencies of conformation and fat'))
      }
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
  freq_input <- (mat_input/total_input)*100
  if(pb_log){
    qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                    paste0('A matrix has been updated based on the frequencies for conformationXfat'))
  }

  ### # vec_rownames content the carcass conformation occuring in the ps_input_flp_tibble
  vec_rownames <- as.numeric(row.names(freq_input))
  vec_colnames <- as.numeric(colnames(freq_input))


  ### # These frequencies are set in a 0-matrix. 
  result_matrix <- matrix(0,nrow = l_constants$fleshiness_scores, ncol = l_constants$fat_scores)
  for(i in 1:nrow(freq_input)){
    x <- vec_rownames[i]
    for(j in 1:ncol(freq_input)){
      y <- vec_colnames[j]
      result_matrix[x,y] <- freq_input[i,j]
    }
  }

  return(result_matrix)
  if(pb_log){
    qp4ewc_log_info(lgr, 'build_freq_conf_fat',
                    paste0('A matrix with all classes for conformationXfat has been set up'))
  }

}


#' @title Transformation of the fleshiness score
#'
#' @description
#' The raw fleshiness score need to be transformed like in the genetic evaluation.
#'
#' @param ps_input_merged_tibble input merged tibble with flp and pedigree data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#'
#' @return tbl_merged_data tibble with a further column containing transformed fleshiness score
#'
#' @export transform_fleshinesss
transform_fleshinesss <- function(ps_input_merged_tibble,
                                  pb_log,
                                  plogger = NULL){
  
  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'transform_fleshinesss.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'transform_fleshinesss',
                    paste0('Starting function with parameters:\n * ps_input_merged_tibble \n'))
  }
  
  
  ### # Get the constants
  l_constants_transform_fleshiness <- get_constants_transform_fleshiness()
  
  
  ### # Add a column for the transformed fleshiness score in line with the breeding value estimation
  ps_input_merged_tibble$Fleshiness_transform <- NA
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_C] <- l_constants_transform_fleshiness$transform_flesh_C
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_H] <- l_constants_transform_fleshiness$transform_flesh_H
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_Tplus] <- l_constants_transform_fleshiness$transform_flesh_Tplus
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_T] <- l_constants_transform_fleshiness$transform_flesh_T
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_Tminus] <- l_constants_transform_fleshiness$transform_flesh_Tminus
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_A] <- l_constants_transform_fleshiness$transform_flesh_A
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_X] <- l_constants_transform_fleshiness$transform_flesh_X
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_XX] <- l_constants_transform_fleshiness$transform_flesh_XX
  ps_input_merged_tibble$Fleshiness_transform[ps_input_merged_tibble$`Fleischigkeit (1. Teil Handelsklasse CHTAX)` %in% l_constants_transform_fleshiness$raw_flesh_XXX] <- l_constants_transform_fleshiness$transform_flesh_XXX
  
  
  return(ps_input_merged_tibble)
  
  
}

