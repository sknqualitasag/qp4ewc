### #
### #
### #
### #   Purpose:   Function related to the pre-processing steps
### #   started:   2022-03-09 (skn)
### #
### # ##################################################################### ###


#' @title Pre-processing the input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_literature path to file with input coming from literature for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_par path to file with input as parameter for each scenanario for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_testedbulls path to file with input for ProdSyst 1 (tested bulls) for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_purchasedreplacementheifers path to file with input for ProdSyst 3 (purchased replacement heifers) for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_date setting the start of the date to filter in the data
#' @param ps_end_date setting the end of the date to filter in the data
#' @param ps_input_file_progeny_flp_statement path to file with statement based on carcass for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from carcass for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp_carcass_matrix_statement path to file with statement based on carcass frequency and price for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_price_cow path to file with price for cows
#' @param ps_input_file_price_bull path to file with price for bulls
#' @param ps_input_file_price_heifer path to file with price for heifers
#' @param ps_input_file_price_calf path to file with price for calf
#' @param ps_input_file_ped path to pedigree file
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ew_input
pre_process_ew_input <- function(ps_sirebreed,
                                 ps_dambreed,
                                 ps_prodsystew,
                                 ps_marketchannel,
                                 ps_path_directory2create,
                                 ps_input_file_literature,
                                 ps_input_file_par,
                                 ps_input_file_testedbulls,
                                 ps_input_file_purchasedreplacementheifers,
                                 ps_input_file_calving_statement,
                                 ps_input_file_calving,
                                 ps_start_date,
                                 ps_end_date,
                                 ps_input_file_progeny_flp_statement,
                                 ps_input_file_flp,
                                 ps_input_file_flp_carcass_matrix_statement,
                                 ps_input_file_price_cow,
                                 ps_input_file_price_bull,
                                 ps_input_file_price_heifer,
                                 ps_input_file_price_calf,
                                 ps_input_file_ped,
                                 pb_log,
                                 plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_literature: ', ps_input_file_literature, '\n',
                           ' * ps_input_file_par: ',ps_input_file_par,'\n',
                           ' * ps_input_file_testedbulls: ',ps_input_file_testedbulls,'\n',
                           ' * ps_input_file_purchasedreplacementheifers: ',ps_input_file_purchasedreplacementheifers,'\n',
                           ' * ps_input_file_calving_statement: ',ps_input_file_calving_statement, '\n',
                           ' * ps_input_file_calving: ', ps_input_file_calving, '\n',
                           ' * ps_start_date: ',ps_start_date,'\n',
                           ' * ps_end_date: ',ps_end_date,'\n',
                           ' * ps_input_file_progeny_flp_statement: ',ps_input_file_progeny_flp_statement,'\n',
                           ' * ps_input_file_flp: ',ps_input_file_flp, '\n',
                           ' * ps_input_file_flp_carcass_matrix_statement: ',ps_input_file_flp_carcass_matrix_statement,'\n',
                           ' * ps_input_file_price_cow: ',ps_input_file_price_cow,'\n',
                           ' * ps_input_file_price_bull: ',ps_input_file_price_bull,'\n',
                           ' * ps_input_file_price_calf: ',ps_input_file_price_calf,'\n',
                           ' * ps_input_file_price_heifer: ',ps_input_file_price_heifer,'\n'))
  }


  ### # Major step 1 ### #
  ### # Create directory per scenario with input-parameter-file for ECOWEIGHT
  create_directory_scenario(ps_sirebreed = ps_sirebreed,
                            ps_dambreed = ps_dambreed,
                            ps_prodsystew = ps_prodsystew,
                            ps_marketchannel = ps_marketchannel,
                            ps_path_directory2create = ps_path_directory2create,
                            pb_log = pb_log,
                            plogger = lgr)


  ### # Get the constants
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  l_constants_ew_input_beefOndairy <- get_constants_ew_input_beefOndairy()


 # ****************************************************************************
 ## ---- Literature ----
 # ****************************************************************************
 ### # Major step 2 ### #
 ### # Read file with input from literature research for input-parameter-file of ECOWEIGHT
 tbl_input_literature <- read_file_input(ps_input_file = ps_input_file_literature,
                                         pb_log = pb_log,
                                         plogger = lgr)
 ### # Update input-parameter-file coming from literature of ECOWEIGHT
 if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
 if(ps_sirebreed == "AN"){
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                 paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                 l_constants_ewbc_input_beefOnbeef$file_av_price_breeding_bull),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull,
                               ps_value2update = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull_AN,
                               pb_log = pb_log,
                               plogger = lgr)
}else if(ps_sirebreed == "AU" || ps_sirebreed == "OB" || ps_sirebreed == "CH"){
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_av_price_breeding_bull),
                              ps_statement2search = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull,
                              ps_value2update = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull_AUCHOB,
                              pb_log = pb_log,
                              plogger = lgr)
}else if(ps_sirebreed == "LM"){
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_av_price_breeding_bull),
                              ps_statement2search = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull,
                              ps_value2update = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull_LM,
                              pb_log = pb_log,
                              plogger = lgr)
}else if(ps_sirebreed == "SI"){
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_av_price_breeding_bull),
                              ps_statement2search = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull,
                              ps_value2update = l_constants_ewbc_input_beefOnbeef$av_price_breeding_bull_SI,
                              pb_log = pb_log,
                              plogger = lgr)
 }else{
   qp4ewc_log_info(lgr, 'pre_process_ewbc_input',
                   paste0('Price for sire breed other than AN, AU, CH, LM, SI, OB are not available for EWBC, please check!'))
 }
 }





   for(l in 1:nrow(tbl_input_literature)){
   if(pb_log){
     qp4ewc_log_info(lgr, 'pre_process_ew_input',
                     paste0('Updating parameter with input coming from the literature file:\n * line number l: ', l, '\n'))
   }
   if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
     idx_col_input_file <- l_constants_ewbc_input_beefOnbeef$idx_col_input_file
     idx_col_input <- l_constants_ewbc_input_beefOnbeef$idx_col_input
     idx_col_input_value <- l_constants_ewbc_input_beefOnbeef$idx_col_input_value
   }else{
     idx_col_input_file <- l_constants_ew_input_beefOndairy$idx_col_input_file
     idx_col_input <- l_constants_ew_input_beefOndairy$idx_col_input
     idx_col_input_value <- l_constants_ew_input_beefOndairy$idx_col_input_value
   }
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                 paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                 tbl_input_literature[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_literature[l,l_constants_ewbc_input_beefOnbeef$idx_col_input],
                               ps_value2update = tbl_input_literature[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_value]$input_value,
                               pb_log = pb_log,
                               plogger = lgr)
 }


 ### # Read file with input to set parameter per scenario for input-parameter-file of ECOWEIGHT
 tbl_input_par <- read_file_input(ps_input_file = ps_input_file_par,
                                  pb_log = pb_log,
                                  plogger = lgr)
 ### # Update input-parameter-file coming from parameter of ECOWEIGHT
 if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
   ### # ps_prodsystew = 1: Closed purebred beef cow herd with pasture system producing males and females for replacement
   if(ps_prodsystew == as.character(1)){
     prodsyst <- l_constants_ewbc_input_beefOnbeef$prodsyst1
     ### # ps_prodsystew = 2: Closed crossbred cow herd with pasture system producing its own female replacement but buying the breeding bulls
   }else if(ps_prodsystew == as.character(2)){
     prodsyst <- l_constants_ewbc_input_beefOnbeef$prodsyst2
     ### # ps_prodsystew = 3: Open beef x dairy or beef x dual purpose crossbred cow herd with pasture system with purchase of cow and bull replacement
   }else if(ps_prodsystew == as.character(3)){
     prodsyst <- l_constants_ewbc_input_beefOnbeef$prodsyst3
   }else{
     qp4ewc_log_info(lgr, 'pre_process_ew_input',
                     paste0('Production System is not 1 or 2 or 3 for EWBC, please use an other function as pre_process_ew_input()!'))
   }
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_par),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$prodsyst,
                               ps_value2update = prodsyst,
                               pb_log = pb_log,
                               plogger = lgr)




 ### # Maturity type of progeny depends on ps_sirebreed
 ### # 1 = Early = AN
 if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
   if(ps_sirebreed == "AN"){
     maturitytype_status <- l_constants_ewbc_input_beefOnbeef$maturitytype_early
   ### # 2 = Medium = AU, OB
   }else if(ps_sirebreed == "AU" || ps_sirebreed == "OB"){
     maturitytype_status <- l_constants_ewbc_input_beefOnbeef$maturitytype_medium
   ### # 3 = Late = CH, LM, SI
   }else if(ps_sirebreed == "CH" || ps_sirebreed == "LM" || ps_sirebreed == "SI"){
     maturitytype_status <- l_constants_ewbc_input_beefOnbeef$maturitytype_late
   }else{
     qp4ewc_log_info(lgr, 'pre_process_ew_input',
                     paste0('Sire breed is not AN, AU, CH, LM, SI, OB for EW, please check!'))
   }
 }
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_par),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$maturitytype,
                               ps_value2update = maturitytype_status,
                               pb_log = pb_log,
                               plogger = lgr)

 }

 ### # Maturity type for production system 4 requires inputs for the purebred dairy breed and crossbred beef-on-dairy
   if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){
     if(ps_dambreed == "HO"){
       maturitytype_status_purebred <- l_constants_ew_input_beefOndairy$maturitytype_early
       if(ps_sirebreed == "AN") {
         maturitytype_status_crossbred <- l_constants_ew_input_beefOndairy$maturitytype_early
       } else if (ps_sirebreed == "LM" || ps_sirebreed == "SI" || ps_sirebreed == "OB") {
         maturitytype_status_crossbred <- l_constants_ew_input_beefOndairy$maturitytype_medium
       }
     }
     if(ps_dambreed == "BS"){
       maturitytype_status_purebred <- l_constants_ew_input_beefOndairy$maturitytype_medium
       if(ps_sirebreed == "AN") {
         maturitytype_status_crossbred <- l_constants_ew_input_beefOndairy$maturitytype_early
       } else if (ps_sirebreed == "LM" || ps_sirebreed == "SI" || ps_sirebreed == "OB") {
         maturitytype_status_crossbred <- l_constants_ew_input_beefOndairy$maturitytype_medium
       }
     }



     update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ew_input_beefOndairy$file_par),
                                 ps_statement2search = l_constants_ew_input_beefOndairy$maturitytype_purebred,
                                 ps_value2update = maturitytype_status_purebred,
                                 pb_log = pb_log,
                                 plogger = lgr)

     update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ew_input_beefOndairy$file_par),
                                 ps_statement2search = l_constants_ew_input_beefOndairy$maturitytype_crossbred,
                                 ps_value2update = maturitytype_status_crossbred,
                                 pb_log = pb_log,
                                 plogger = lgr)
   }



 ### # Mating type for heifers or cows
 ### # 1 : Artificial insemination is used in the first oestrus within one mating period
 if(ps_prodsystew == as.character(1) || ps_prodsystew == as.character(2)){
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_par),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$matingtype_heifer,
                               ps_value2update = l_constants_ewbc_input_beefOnbeef$matingtype_AI,
                               pb_log = pb_log,
                               plogger = lgr)

   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_par),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$matingtype_cow,
                               ps_value2update = l_constants_ewbc_input_beefOnbeef$matingtype_AI,
                               pb_log = pb_log,
                               plogger = lgr)
 ### # 2 : Natural mating is used throughout.
 }else if(ps_prodsystew == as.character(3)){
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_par),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$matingtype_heifer,
                               ps_value2update = l_constants_ewbc_input_beefOnbeef$matingtype_NM,
                               pb_log = pb_log,
                               plogger = lgr)

   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),l_constants_ewbc_input_beefOnbeef$file_par),
                               ps_statement2search = l_constants_ewbc_input_beefOnbeef$matingtype_cow,
                               ps_value2update = l_constants_ewbc_input_beefOnbeef$matingtype_NM,
                               pb_log = pb_log,
                               plogger = lgr)
 }


 for(l in 1:nrow(tbl_input_par)){
   if(pb_log){
     qp4ewc_log_info(lgr, 'pre_process_ew_input',
                     paste0('Updating parameter with input coming from the parameter file:\n * line number l: ', l, '\n'))
   }
   if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
     idx_col_input_file <- l_constants_ewbc_input_beefOnbeef$idx_col_input_file
     idx_col_input <- l_constants_ewbc_input_beefOnbeef$idx_col_input
     idx_col_input_value <- l_constants_ewbc_input_beefOnbeef$idx_col_input_value
   }else{
     idx_col_input_file <- l_constants_ew_input_beefOndairy$idx_col_input_file
     idx_col_input <- l_constants_ew_input_beefOndairy$idx_col_input
     idx_col_input_value <- l_constants_ew_input_beefOndairy$idx_col_input_value
   }
   update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                 paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                 tbl_input_par[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_par[l,l_constants_ewbc_input_beefOnbeef$idx_col_input],
                                ps_value2update = tbl_input_par[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_value]$input_value,
                                pb_log = pb_log,
                                plogger = lgr)
 }


 ### # If productionsystem 1 : Read file with input for tested bulls for input-parameter-file of ECOWEIGHT
 if(ps_prodsystew == as.character(1)){
   tbl_input_testedbull <- read_file_input(ps_input_file = ps_input_file_testedbulls,
                                           pb_log = pb_log,
                                           plogger = lgr)

   for(l in 1:nrow(tbl_input_testedbull)){
     if(pb_log){
       qp4ewc_log_info(lgr, 'pre_process_ew_input',
                       paste0('Updating parameter with input coming from the parameter file:\n * line number m: ', l, '\n'))
     }
     update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                   paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                   tbl_input_testedbull[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_file]),
                                 ps_statement2search = tbl_input_testedbull[l,l_constants_ewbc_input_beefOnbeef$idx_col_input],
                                 ps_value2update = tbl_input_testedbull[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_value]$input_value,
                                 pb_log = pb_log,
                                 plogger = lgr)
   }
 }


 ### # If productionsystem 3 :Read file with input from purchased replacement heifers for input-parameter-file of ECOWEIGHT
 if(ps_prodsystew == as.character(3)){
   tbl_input_purchasedreplacementheifers <- read_file_input(ps_input_file = ps_input_file_purchasedreplacementheifers,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
   for(l in 1:nrow(tbl_input_purchasedreplacementheifers)){
     if(pb_log){
       qp4ewc_log_info(lgr, 'pre_process_ew_input',
                       paste0('Updating parameter with input coming from the parameter file:\n * line number l: ', l, '\n'))
     }
     update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                   paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                   tbl_input_purchasedreplacementheifers[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_file]),
                                 ps_statement2search = tbl_input_purchasedreplacementheifers[l,l_constants_ewbc_input_beefOnbeef$idx_col_input],
                                 ps_value2update = tbl_input_purchasedreplacementheifers[l,l_constants_ewbc_input_beefOnbeef$idx_col_input_value]$input_value,
                                 pb_log = pb_log,
                                 plogger = lgr)
   }
 }




  # ****************************************************************************
  ## ---- Calving ----
  # ****************************************************************************
  ### # Major step 3 ### #
  ### # Pre-processing the calving data for input-parameter-file of ECOWEIGHT
  pre_process_ew_input_calving(ps_sirebreed = ps_sirebreed,
                               ps_dambreed = ps_dambreed,
                               ps_prodsystew = ps_prodsystew,
                               ps_marketchannel = ps_marketchannel,
                               ps_path_directory2create = ps_path_directory2create,
                               ps_input_file_calving_statement = ps_input_file_calving_statement,
                               ps_input_file_calving = ps_input_file_calving,
                               ps_start_calving_date = ps_start_date,
                               ps_end_calving_date = ps_end_date,
                               pb_log = pb_log,
                               plogger = lgr)


  if(pb_log){
    qp4ewc_log_info(lgr, 'pre_process_ew_input',
                    paste0('Updating parameter with input coming from the calving file '))
  }


 # ****************************************************************************
 ## ---- Progreny data about weighing and slaughter ----
 # ****************************************************************************
 ### # Major step 4 ### #
 ### # production system beef-on-beef
 if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
   pre_process_ewbc_input_progeny_data_flp(ps_sirebreed = ps_sirebreed,
                                           ps_dambreed = ps_dambreed,
                                           ps_prodsystew = ps_prodsystew,
                                           ps_marketchannel = ps_marketchannel,
                                           ps_path_directory2create = ps_path_directory2create,
                                           ps_input_file_progeny_flp_statement = ps_input_file_progeny_flp_statement,
                                           ps_input_file_flp = ps_input_file_flp,
                                           ps_start_flp_date = ps_start_date,
                                           ps_end_flp_date = ps_end_date,
                                           ps_input_file_ped = ps_input_file_ped,
                                           pb_log = pb_log,
                                           plogger = lgr)
 }else{
   ### # production system beef-on-dairy
   pre_process_ewdc_input_progeny_data_flp(ps_sirebreed = ps_sirebreed,
                                           ps_dambreed = ps_dambreed,
                                           ps_prodsystew = ps_prodsystew,
                                           ps_marketchannel = ps_marketchannel,
                                           ps_path_directory2create = ps_path_directory2create,
                                           ps_input_file_progeny_flp_statement = ps_input_file_progeny_flp_statement,
                                           ps_input_file_flp = ps_input_file_flp,
                                           ps_start_flp_date = ps_start_date,
                                           ps_end_flp_date = ps_end_date,
                                           ps_input_file_ped = ps_input_file_ped,
                                           pb_log = pb_log,
                                           plogger = lgr)
 }


  if(pb_log){
    qp4ewc_log_info(lgr, 'pre_process_ew_input',
                    paste0('Updating parameter with input coming from the progeny data flp file '))
  }


 # ****************************************************************************
 ## ---- Conformation & fat scores and price matrices ----
 # ****************************************************************************
 ### # Major step 5 ### #
if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
 pre_process_ewbc_input_carcass_data_flp(ps_sirebreed = ps_sirebreed,
                                         ps_dambreed = ps_dambreed,
                                         ps_prodsystew = ps_prodsystew,
                                         ps_marketchannel = ps_marketchannel,
                                         ps_path_directory2create = ps_path_directory2create,
                                         ps_input_file_flp_carcass_matrix_statement = ps_input_file_flp_carcass_matrix_statement,
                                         ps_input_file_flp = ps_input_file_flp,
                                         ps_input_file_ped = ps_input_file_ped,
                                         ps_start_flp_date = ps_start_date,
                                         ps_end_flp_date = ps_end_date,
                                         ps_input_file_price_cow = ps_input_file_price_cow,
                                         ps_input_file_price_bull = ps_input_file_price_bull,
                                         ps_input_file_price_heifer = ps_input_file_price_heifer,
                                         pb_log = pb_log,
                                         plogger = lgr)
}else{
  ### # production system beef-on-dairy
  pre_process_ewdc_input_carcass_data_flp(ps_sirebreed = ps_sirebreed,
                                          ps_dambreed = ps_dambreed,
                                          ps_prodsystew = ps_prodsystew,
                                          ps_marketchannel = ps_marketchannel,
                                          ps_path_directory2create = ps_path_directory2create,
                                          ps_input_file_flp_carcass_matrix_statement = ps_input_file_flp_carcass_matrix_statement,
                                          ps_input_file_flp = ps_input_file_flp,
                                          ps_input_file_ped = ps_input_file_ped,
                                          ps_start_flp_date = ps_start_date,
                                          ps_end_flp_date = ps_end_date,
                                          ps_input_file_price_cow = ps_input_file_price_cow,
                                          ps_input_file_price_bull = ps_input_file_price_bull,
                                          ps_input_file_price_heifer = ps_input_file_price_heifer,
                                          ps_input_file_price_calf = ps_input_file_price_calf,
                                          pb_log = pb_log,
                                          plogger = lgr)
}



  if(pb_log){
    qp4ewc_log_info(lgr, 'pre_process_ew_input',
                    paste0('Updating parameter with input coming from the conformation & fat scores, prices '))
  }

}



#' @title Pre-processing the calving data for input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on calving data.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_calving_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_calving path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_calving_date setting the start of the calving date to filter in the calving data
#' @param ps_end_calving_date setting the end of the calving date to filter in the calving data
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @export pre_process_ew_input_calving
pre_process_ew_input_calving <- function(ps_sirebreed,
                                         ps_dambreed,
                                         ps_prodsystew,
                                         ps_marketchannel,
                                         ps_path_directory2create,
                                         ps_input_file_calving_statement,
                                         ps_input_file_calving,
                                         ps_start_calving_date,
                                         ps_end_calving_date,
                                         pb_log,
                                         plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input_calving.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input_calving',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_calving_statement: ',ps_input_file_calving_statement, '\n',
                           ' * ps_input_file_calving: ', ps_input_file_calving, '\n',
                           ' * ps_start_calving_date: ',ps_start_calving_date,'\n',
                           ' * ps_end_calving_date: ',ps_end_calving_date,'\n'))
  }


  ### # Read file with statement based on calving for input-parameter-file of ECOWEIGHT
  tbl_input_statement_calving <- read_file_input(ps_input_file = ps_input_file_calving_statement,
                                                 pb_log = pb_log,
                                                 plogger = lgr)


  ### # Read file with calving data
  tbl_calving <- read_file_input_calving(ps_input_file_calving = ps_input_file_calving,
                                         ps_start_calving_date = ps_start_calving_date,
                                         ps_end_calving_date = ps_end_calving_date,
                                         pb_log = pb_log,
                                         plogger = lgr)

  ### # Get the constants for calving
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  l_constants_ew_input_beefOndairy <- get_constants_ew_input_beefOndairy()
  l_constants_calving_beefOnbeef <- get_constants_calving_beefOnbeef()
  l_constants_calving_beefOndairy <- get_constants_calving_beefOndairy()

  # Update statement-calving-input from the data by calculating abortion rate
  abortrate_prim <- calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                            ps_statement_firstlactation = TRUE,
                                            pb_log = pb_log,
                                            plogger = lgr)

  abortrate_multi <- calculate_abortion_rate(ps_input_calving_tibble = tbl_calving,
                                             ps_statement_firstlactation = FALSE,
                                             pb_log = pb_log,
                                             plogger = lgr)

  #updates depending on production system
  if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_abortrate <- l_constants_calving_beefOnbeef$idx_row_abortrate
    first_element_vector <- l_constants_calving_beefOnbeef$first_element_vector
    idx_col_input_value <- l_constants_calving_beefOnbeef$idx_col_input_value
    idx_col_input <- l_constants_calving_beefOnbeef$idx_col_input
    idx_col_input_file <- l_constants_calving_beefOnbeef$idx_col_input_file
    second_element_vector <- l_constants_calving_beefOnbeef$second_element_vector
  }
  if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_abortrate <- l_constants_calving_beefOndairy$idx_row_abortrate
    first_element_vector <- l_constants_calving_beefOndairy$first_element_vector
    idx_col_input_value <- l_constants_calving_beefOndairy$idx_col_input_value
    idx_col_input <- l_constants_calving_beefOndairy$idx_col_input
    idx_col_input_file <- l_constants_calving_beefOndairy$idx_col_input_file
    second_element_vector <- l_constants_calving_beefOndairy$second_element_vector
  }


   # Check if abortrate_prim is zero. If it is the case, set a default value
  if(abortrate_prim == 0){
    abortrate_prim <- unlist(strsplit(tbl_input_statement_calving[idx_row_abortrate,idx_row_abortrate]$input_value,
                                      split = " "))[first_element_vector]
  }

  # Check if abortrate_multi is zero. If it is the case, set a default value
  if(abortrate_multi == 0){
    abortrate_multi <- unlist(strsplit(tbl_input_statement_calving[idx_row_abortrate,idx_col_input_value]$input_value,
                                       split = " "))[second_element_vector]
  }
  value2update_abortrate <- paste0(c(abortrate_prim, rep(abortrate_multi,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_abortrate,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_abortrate,idx_col_input],
                              ps_value2update = value2update_abortrate,
                              pb_log = pb_log,
                              plogger = lgr)


  ### # Update statement-calving-input from the data by calculating stillbirth rate
  # stillbirth rate easy calving
  stillbirthrate_prim_easy <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                        ps_statement_firstlactation = TRUE,
                                                        ps_statement_easycalving = TRUE,
                                                        pb_log = pb_log,
                                                        plogger = lgr)
  stillbirthrate_multi_easy <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                         ps_statement_firstlactation = FALSE,
                                                         ps_statement_easycalving = TRUE,
                                                         pb_log = pb_log,
                                                         plogger = lgr)

  #updates depending on production system
  if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_stillborn_easy <- l_constants_calving_beefOnbeef$idx_row_stillborn_easy
    idx_col_input_value <- l_constants_calving_beefOnbeef$idx_col_input_value
    first_element_vector <- l_constants_calving_beefOnbeef$first_element_vector
    second_element_vector <- l_constants_calving_beefOnbeef$second_element_vector
    idx_col_input_file <- l_constants_calving_beefOnbeef$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOnbeef$idx_col_input
    idx_row_stillborn_diff <- l_constants_calving_beefOnbeef$idx_row_stillborn_diff
  }

  if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_stillborn_easy <- l_constants_calving_beefOndairy$idx_row_stillborn_easy
    idx_col_input_value <- l_constants_calving_beefOndairy$idx_col_input_value
    first_element_vector <- l_constants_calving_beefOndairy$first_element_vector
    second_element_vector <- l_constants_calving_beefOndairy$second_element_vector
    idx_col_input_file <- l_constants_calving_beefOndairy$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOndairy$idx_col_input
    idx_row_stillborn_diff <- l_constants_calving_beefOndairy$idx_row_stillborn_diff
  }

  # Check if stillbirthrate_prim_easy is zero. If it is the case, set a default value
  if(stillbirthrate_prim_easy == 0){
    stillbirthrate_prim_easy <- unlist(strsplit(tbl_input_statement_calving[idx_row_stillborn_easy,idx_col_input_value]$input_value,
                                                split = " "))[first_element_vector]
  }

  # Check if stillbirthrate_multi_easy is zero. If it is the case, set a default value
  if(stillbirthrate_multi_easy == 0){
    stillbirthrate_multi_easy <- unlist(strsplit(tbl_input_statement_calving[idx_row_stillborn_easy,idx_col_input_value]$input_value,
                                                 split = " "))[second_element_vector]
  }
  value2update_stillbirthrate_easy <- paste0(c(stillbirthrate_prim_easy, rep(stillbirthrate_multi_easy,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_stillborn_easy,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_stillborn_easy,idx_col_input],
                              ps_value2update = value2update_stillbirthrate_easy,
                              pb_log = pb_log,
                              plogger = lgr)

# stillbirth rate difficult calving
  stillbirthrate_prim_difficult <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                             ps_statement_firstlactation = TRUE,
                                                             ps_statement_easycalving = FALSE,
                                                             pb_log = pb_log,
                                                             plogger = lgr)

  stillbirthrate_multi_difficult <- calculate_stillbirth_rate(ps_input_calving_tibble = tbl_calving,
                                                              ps_statement_firstlactation = FALSE,
                                                              ps_statement_easycalving = FALSE,
                                                              pb_log = pb_log,
                                                              plogger = lgr)
  # Check if stillbirthrate_prim_difficult is zero. If it is the case, set a default value
  if(stillbirthrate_prim_difficult == 0){
    stillbirthrate_prim_difficult <- unlist(strsplit(tbl_input_statement_calving[idx_row_stillborn_diff,idx_col_input_value]$input_value,
                                                     split = " "))[first_element_vector]
  }
  # Check if stillbirthrate_multi_difficult is zero. If it is the case, set a default value
  if(stillbirthrate_multi_difficult == 0){
    stillbirthrate_multi_difficult <- unlist(strsplit(tbl_input_statement_calving[idx_row_stillborn_diff,idx_col_input_value]$input_value,
                                                      split = " "))[second_element_vector]
  }
  value2update_stillbirthrate_difficult <- paste0(c(stillbirthrate_prim_difficult, rep(stillbirthrate_multi_difficult,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_stillborn_diff,idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_calving[idx_row_stillborn_diff,idx_col_input],
                               ps_value2update = value2update_stillbirthrate_difficult,
                               pb_log = pb_log,
                               plogger = lgr)


  ### # Update calving score parameter inputs
  #updates depending on production system
  if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_class_calving <- l_constants_calving_beefOnbeef$idx_row_class_calving
    idx_col_input_file <- l_constants_calving_beefOnbeef$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOnbeef$idx_col_input
    n_class_calving <- l_constants_calving_beefOnbeef$n_class_calving
    idx_row_class_dystocia <- l_constants_calving_beefOnbeef$idx_row_class_dystocia
    class_dystocia <- l_constants_calving_beefOndairy$class_dystocia
  }

  if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_class_calving <- l_constants_calving_beefOndairy$idx_row_class_calving
    idx_col_input_file <- l_constants_calving_beefOndairy$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOndairy$idx_col_input
    n_class_calving <- l_constants_calving_beefOndairy$n_class_calving
    idx_row_class_dystocia <- l_constants_calving_beefOndairy$idx_row_class_dystocia
    class_dystocia <- l_constants_calving_beefOndairy$class_dystocia
  }
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_class_calving,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_class_calving,idx_col_input],
                              ps_value2update = n_class_calving,
                              pb_log = pb_log,
                              plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_class_dystocia,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_class_dystocia,idx_col_input],
                              ps_value2update = class_dystocia,
                              pb_log = pb_log,
                              plogger = lgr)


  ### # Update calving score proportions
  #updates depending on production system
  if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
    sex_female <- l_constants_calving_beefOnbeef$sex_female
    sex_male <- l_constants_calving_beefOnbeef$sex_male
    calvingscore2 <- l_constants_calving_beefOnbeef$calvingscore2
    calvingscore3 <- l_constants_calving_beefOnbeef$calvingscore3
    calvingscore4 <- l_constants_calving_beefOnbeef$calvingscore4
    idx_row_calvingscore2_female <- l_constants_calving_beefOnbeef$idx_row_calvingscore2_female
    idx_row_calvingscore3_female <- l_constants_calving_beefOnbeef$idx_row_calvingscore3_female
    idx_row_calvingscore4_female <- l_constants_calving_beefOnbeef$idx_row_calvingscore4_female
    idx_row_calvingscore2_male <- l_constants_calving_beefOnbeef$idx_row_calvingscore2_male
    idx_row_calvingscore3_male <- l_constants_calving_beefOnbeef$idx_row_calvingscore3_male
    idx_row_calvingscore4_male <- l_constants_calving_beefOnbeef$idx_row_calvingscore4_male
    idx_col_input_file <- l_constants_calving_beefOnbeef$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOnbeef$idx_col_input
      }

  if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){
    sex_female <- l_constants_calving_beefOndairy$sex_female
    sex_male <- l_constants_calving_beefOndairy$sex_male
    calvingscore2 <- l_constants_calving_beefOndairy$calvingscore2
    calvingscore3 <- l_constants_calving_beefOndairy$calvingscore3
    calvingscore4 <- l_constants_calving_beefOndairy$calvingscore4
    idx_row_calvingscore2_female <- l_constants_calving_beefOndairy$idx_row_calvingscore2_female
    idx_row_calvingscore3_female <- l_constants_calving_beefOndairy$idx_row_calvingscore3_female
    idx_row_calvingscore4_female <- l_constants_calving_beefOndairy$idx_row_calvingscore4_female
    idx_row_calvingscore2_male <- l_constants_calving_beefOndairy$idx_row_calvingscore2_male
    idx_row_calvingscore3_male <- l_constants_calving_beefOndairy$idx_row_calvingscore3_male
    idx_row_calvingscore4_male <- l_constants_calving_beefOndairy$idx_row_calvingscore4_male
    idx_col_input_file <- l_constants_calving_beefOndairy$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOndairy$idx_col_input
  }

  calvingscore_prop_prim_F_2 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = sex_female,
                                                                  ps_calvingscore = calvingscore2,
                                                                  ps_sirebreed = ps_sirebreed,
                                                                  ps_dambreed = ps_dambreed,
                                                                  pb_log = pb_log,
                                                                  plogger = lgr)
  calvingscore_prop_prim_F_3 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = sex_female,
                                                                  ps_calvingscore = calvingscore3,
                                                                  ps_sirebreed = ps_sirebreed,
                                                                  ps_dambreed = ps_dambreed,
                                                                  pb_log = pb_log,
                                                                  plogger = lgr)
  calvingscore_prop_prim_F_4 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = sex_female,
                                                                  ps_calvingscore = calvingscore4,
                                                                  ps_sirebreed = ps_sirebreed,
                                                                  ps_dambreed = ps_dambreed,
                                                                  pb_log = pb_log,
                                                                  plogger = lgr)
  calvingscore_prop_multi_F_2 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = sex_female,
                                                                   ps_calvingscore = calvingscore2,
                                                                   ps_sirebreed = ps_sirebreed,
                                                                   ps_dambreed = ps_dambreed,
                                                                   pb_log = pb_log,
                                                                   plogger = lgr)
  calvingscore_prop_multi_F_3 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = sex_female,
                                                                   ps_calvingscore = calvingscore3,
                                                                   ps_sirebreed = ps_sirebreed,
                                                                   ps_dambreed = ps_dambreed,
                                                                   pb_log = pb_log,
                                                                   plogger = lgr)
  calvingscore_prop_multi_F_4 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = sex_female,
                                                                   ps_calvingscore = calvingscore4,
                                                                   ps_sirebreed = ps_sirebreed,
                                                                   ps_dambreed = ps_dambreed,
                                                                   pb_log = pb_log,
                                                                   plogger = lgr)
  calvingscore_prop_prim_M_2 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = sex_male,
                                                                  ps_calvingscore = calvingscore2,
                                                                  ps_sirebreed = ps_sirebreed,
                                                                  ps_dambreed = ps_dambreed,
                                                                  pb_log = pb_log,
                                                                  plogger = lgr)
  calvingscore_prop_prim_M_3 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = sex_male,
                                                                  ps_calvingscore = calvingscore3,
                                                                  ps_sirebreed = ps_sirebreed,
                                                                  ps_dambreed = ps_dambreed,
                                                                  pb_log = pb_log,
                                                                  plogger = lgr)
  calvingscore_prop_prim_M_4 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                  ps_statement_firstlactation = TRUE,
                                                                  ps_sex = sex_male,
                                                                  ps_calvingscore = calvingscore4,
                                                                  ps_sirebreed = ps_sirebreed,
                                                                  ps_dambreed = ps_dambreed,
                                                                  pb_log = pb_log,
                                                                  plogger = lgr)
  calvingscore_prop_multi_M_2 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = sex_male,
                                                                   ps_calvingscore = calvingscore2,
                                                                   ps_sirebreed = ps_sirebreed,
                                                                   ps_dambreed = ps_dambreed,
                                                                   pb_log = pb_log,
                                                                   plogger = lgr)
  calvingscore_prop_multi_M_3 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = sex_male,
                                                                   ps_calvingscore = calvingscore3,
                                                                   ps_sirebreed = ps_sirebreed,
                                                                   ps_dambreed = ps_dambreed,
                                                                   pb_log = pb_log,
                                                                   plogger = lgr)
  calvingscore_prop_multi_M_4 <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                   ps_statement_firstlactation = FALSE,
                                                                   ps_sex = sex_male,
                                                                   ps_calvingscore = calvingscore4,
                                                                   ps_sirebreed = ps_sirebreed,
                                                                   ps_dambreed = ps_dambreed,
                                                                   pb_log = pb_log,
                                                                   plogger = lgr)

  value2update_calvingscoreprop_prim_F_2 <- paste0(c(calvingscore_prop_prim_F_2, rep(calvingscore_prop_multi_F_2,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calvingscore2_female,idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore2_female,idx_col_input],
                               ps_value2update = value2update_calvingscoreprop_prim_F_2,
                               pb_log = pb_log,
                               plogger = lgr)
  value2update_calvingscoreprop_prim_F_3 <- paste0(c(calvingscore_prop_prim_F_3, rep(calvingscore_prop_multi_F_3,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calvingscore3_female,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore3_female,idx_col_input],
                              ps_value2update = value2update_calvingscoreprop_prim_F_3,
                              pb_log = pb_log,
                              plogger = lgr)
  value2update_calvingscoreprop_prim_F_4 <- paste0(c(calvingscore_prop_prim_F_4, rep(calvingscore_prop_multi_F_4,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calvingscore4_female,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore4_female,idx_col_input],
                              ps_value2update = value2update_calvingscoreprop_prim_F_4,
                              pb_log = pb_log,
                              plogger = lgr)
  value2update_calvingscoreprop_prim_M_2 <- paste0(c(calvingscore_prop_prim_M_2, rep(calvingscore_prop_multi_M_2,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calvingscore2_male,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore2_male,idx_col_input],
                              ps_value2update = value2update_calvingscoreprop_prim_M_2,
                              pb_log = pb_log,
                              plogger = lgr)
  value2update_calvingscoreprop_prim_M_3 <- paste0(c(calvingscore_prop_prim_M_3, rep(calvingscore_prop_multi_M_3,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calvingscore3_male,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore3_male,idx_col_input],
                              ps_value2update = value2update_calvingscoreprop_prim_M_3,
                              pb_log = pb_log,
                              plogger = lgr)
  value2update_calvingscoreprop_prim_M_4 <- paste0(c(calvingscore_prop_prim_M_4, rep(calvingscore_prop_multi_M_4,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calvingscore4_male,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore4_male,idx_col_input],
                              ps_value2update = value2update_calvingscoreprop_prim_M_4,
                              pb_log = pb_log,
                              plogger = lgr)


  # Beef-on-dairy requires the same information also for purebred dairy animals ps_sirebreed = s_dambreed
  if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){

    sex_female <- l_constants_calving_beefOndairy$sex_female
    sex_male <- l_constants_calving_beefOndairy$sex_male
    calvingscore2 <- l_constants_calving_beefOndairy$calvingscore2
    calvingscore3 <- l_constants_calving_beefOndairy$calvingscore3
    calvingscore4 <- l_constants_calving_beefOndairy$calvingscore4
    idx_row_calvingscore2_female_pure <- l_constants_calving_beefOndairy$idx_row_calvingscore2_female_pure
    idx_row_calvingscore3_female_pure <- l_constants_calving_beefOndairy$idx_row_calvingscore3_female_pure
    idx_row_calvingscore4_female_pure <- l_constants_calving_beefOndairy$idx_row_calvingscore4_female_pure
    idx_row_calvingscore2_male_pure <- l_constants_calving_beefOndairy$idx_row_calvingscore2_male_pure
    idx_row_calvingscore3_male_pure <- l_constants_calving_beefOndairy$idx_row_calvingscore3_male_pure
    idx_row_calvingscore4_male_pure <- l_constants_calving_beefOndairy$idx_row_calvingscore4_male_pure
    idx_col_input_file <- l_constants_calving_beefOndairy$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOndairy$idx_col_input

    calvingscore_prop_prim_F_2_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                         ps_statement_firstlactation = TRUE,
                                                                         ps_sex = sex_female,
                                                                         ps_calvingscore = calvingscore2,
                                                                         ps_sirebreed = ps_dambreed,
                                                                         ps_dambreed = ps_dambreed,
                                                                         pb_log = pb_log,
                                                                         plogger = lgr)
    calvingscore_prop_prim_F_3_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                         ps_statement_firstlactation = TRUE,
                                                                         ps_sex = sex_female,
                                                                         ps_calvingscore = calvingscore3,
                                                                         ps_sirebreed = ps_dambreed,
                                                                         ps_dambreed = ps_dambreed,
                                                                         pb_log = pb_log,
                                                                         plogger = lgr)
    calvingscore_prop_prim_F_4_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                         ps_statement_firstlactation = TRUE,
                                                                         ps_sex = sex_female,
                                                                         ps_calvingscore = calvingscore4,
                                                                         ps_sirebreed = ps_dambreed,
                                                                         ps_dambreed = ps_dambreed,
                                                                         pb_log = pb_log,
                                                                         plogger = lgr)
    calvingscore_prop_multi_F_2_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = FALSE,
                                                                          ps_sex = sex_female,
                                                                          ps_calvingscore = calvingscore2,
                                                                          ps_sirebreed = ps_dambreed,
                                                                          ps_dambreed = ps_dambreed,
                                                                          pb_log = pb_log,
                                                                          plogger = lgr)
    calvingscore_prop_multi_F_3_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = FALSE,
                                                                          ps_sex = sex_female,
                                                                          ps_calvingscore = calvingscore3,
                                                                          ps_sirebreed = ps_dambreed,
                                                                          ps_dambreed = ps_dambreed,
                                                                          pb_log = pb_log,
                                                                          plogger = lgr)
    calvingscore_prop_multi_F_4_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = FALSE,
                                                                          ps_sex = sex_female,
                                                                          ps_calvingscore = calvingscore4,
                                                                          ps_sirebreed = ps_dambreed,
                                                                          ps_dambreed = ps_dambreed,
                                                                          pb_log = pb_log,
                                                                          plogger = lgr)
    calvingscore_prop_prim_M_2_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                         ps_statement_firstlactation = TRUE,
                                                                         ps_sex = sex_male,
                                                                         ps_calvingscore = calvingscore2,
                                                                         ps_sirebreed = ps_dambreed,
                                                                         ps_dambreed = ps_dambreed,
                                                                         pb_log = pb_log,
                                                                         plogger = lgr)
    calvingscore_prop_prim_M_3_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                         ps_statement_firstlactation = TRUE,
                                                                         ps_sex = sex_male,
                                                                         ps_calvingscore = calvingscore3,
                                                                         ps_sirebreed = ps_dambreed,
                                                                         ps_dambreed = ps_dambreed,
                                                                         pb_log = pb_log,
                                                                         plogger = lgr)
    calvingscore_prop_prim_M_4_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                         ps_statement_firstlactation = TRUE,
                                                                         ps_sex = sex_male,
                                                                         ps_calvingscore = calvingscore4,
                                                                         ps_sirebreed = ps_dambreed,
                                                                         ps_dambreed = ps_dambreed,
                                                                         pb_log = pb_log,
                                                                         plogger = lgr)
    calvingscore_prop_multi_M_2_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = FALSE,
                                                                          ps_sex = sex_male,
                                                                          ps_calvingscore = calvingscore2,
                                                                          ps_sirebreed = ps_dambreed,
                                                                          ps_dambreed = ps_dambreed,
                                                                          pb_log = pb_log,
                                                                          plogger = lgr)
    calvingscore_prop_multi_M_3_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = FALSE,
                                                                          ps_sex = sex_male,
                                                                          ps_calvingscore = calvingscore3,
                                                                          ps_sirebreed = ps_dambreed,
                                                                          ps_dambreed = ps_dambreed,
                                                                          pb_log = pb_log,
                                                                          plogger = lgr)
    calvingscore_prop_multi_M_4_pure <- calculate_calvingscore_proportion(ps_input_calving_tibble = tbl_calving,
                                                                          ps_statement_firstlactation = FALSE,
                                                                          ps_sex = sex_male,
                                                                          ps_calvingscore = calvingscore4,
                                                                          ps_sirebreed = ps_dambreed,
                                                                          ps_dambreed = ps_dambreed,
                                                                          pb_log = pb_log,
                                                                          plogger = lgr)

    value2update_calvingscoreprop_prim_F_2_pure <- paste0(c(calvingscore_prop_prim_F_2_pure, rep(calvingscore_prop_multi_F_2_pure,9)),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_calving[idx_row_calvingscore2_female_pure,idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore2_female_pure,idx_col_input],
                                ps_value2update = value2update_calvingscoreprop_prim_F_2_pure,
                                pb_log = pb_log,
                                plogger = lgr)
    value2update_calvingscoreprop_prim_F_3_pure <- paste0(c(calvingscore_prop_prim_F_3_pure, rep(calvingscore_prop_multi_F_3_pure,9)),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_calving[idx_row_calvingscore3_female_pure,idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore3_female_pure,idx_col_input],
                                ps_value2update = value2update_calvingscoreprop_prim_F_3_pure,
                                pb_log = pb_log,
                                plogger = lgr)
    value2update_calvingscoreprop_prim_F_4_pure <- paste0(c(calvingscore_prop_prim_F_4_pure, rep(calvingscore_prop_multi_F_4_pure,9)),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_calving[idx_row_calvingscore4_female_pure,idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore4_female_pure,idx_col_input],
                                ps_value2update = value2update_calvingscoreprop_prim_F_4_pure,
                                pb_log = pb_log,
                                plogger = lgr)
    value2update_calvingscoreprop_prim_M_2_pure <- paste0(c(calvingscore_prop_prim_M_2_pure, rep(calvingscore_prop_multi_M_2_pure,9)),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_calving[idx_row_calvingscore2_male_pure,idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore2_male_pure,idx_col_input],
                                ps_value2update = value2update_calvingscoreprop_prim_M_2_pure,
                                pb_log = pb_log,
                                plogger = lgr)
    value2update_calvingscoreprop_prim_M_3_pure <- paste0(c(calvingscore_prop_prim_M_3_pure, rep(calvingscore_prop_multi_M_3_pure,9)),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_calving[idx_row_calvingscore3_male_pure,idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore3_male_pure,idx_col_input],
                                ps_value2update = value2update_calvingscoreprop_prim_M_3_pure,
                                pb_log = pb_log,
                                plogger = lgr)
    value2update_calvingscoreprop_prim_M_4_pure <- paste0(c(calvingscore_prop_prim_M_4_pure, rep(calvingscore_prop_multi_M_4_pure,9)),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_calving[idx_row_calvingscore4_male_pure,idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_calving[idx_row_calvingscore4_male_pure,idx_col_input],
                                ps_value2update = value2update_calvingscoreprop_prim_M_4_pure,
                                pb_log = pb_log,
                                plogger = lgr)

  }



  ### # Update proportion of calves died to 24 hours
  #updates depending on production system
  if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_calfdied48h_easy <- l_constants_calving_beefOnbeef$idx_row_calfdied48h_easy
    idx_col_input_value <- l_constants_calving_beefOnbeef$idx_col_input_value
    first_element_vector <- l_constants_calving_beefOnbeef$first_element_vector
    second_element_vector <- l_constants_calving_beefOnbeef$second_element_vector
    idx_col_input_file <- l_constants_calving_beefOnbeef$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOnbeef$idx_col_input
    idx_row_calfdied48h_dystocia <- l_constants_calving_beefOnbeef$idx_row_calfdied48h_dystocia
  }

  if(ps_prodsystew == l_constants_ew_input_beefOndairy$prodsyst4){
    idx_row_calfdied48h_easy <- l_constants_calving_beefOndairy$idx_row_calfdied48h_easy
    idx_col_input_value <- l_constants_calving_beefOndairy$idx_col_input_value
    first_element_vector <- l_constants_calving_beefOndairy$first_element_vector
    second_element_vector <- l_constants_calving_beefOndairy$second_element_vector
    idx_col_input_file <- l_constants_calving_beefOndairy$idx_col_input_file
    idx_col_input <- l_constants_calving_beefOndairy$idx_col_input
    idx_row_calfdied48h_dystocia <- l_constants_calving_beefOndairy$idx_row_calfdied48h_dystocia
  }

  calvingdied24h_prop_prim_easy <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                      ps_statement_firstlactation = TRUE,
                                                                      ps_statement_easycalving = TRUE,
                                                                      pb_log = pb_log,
                                                                      plogger = lgr)

  calvingdied24h_prop_multi_easy <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                       ps_statement_firstlactation = FALSE,
                                                                       ps_statement_easycalving = TRUE,
                                                                       pb_log = pb_log,
                                                                       plogger = lgr)

  # Check if calvingdied24h_prop_prim_easy is zero. If it is the case, set a default value
  if(calvingdied24h_prop_prim_easy == 0){
    calvingdied24h_prop_prim_easy <- unlist(strsplit(tbl_input_statement_calving[idx_row_calfdied48h_easy,idx_col_input_value]$input_value,
                                                     split = " "))[first_element_vector]
  }

  # Check if calvingdied24h_prop_multi_easy is zero. If it is the case, set a default value
  if(calvingdied24h_prop_multi_easy == 0){
    calvingdied24h_prop_multi_easy <- unlist(strsplit(tbl_input_statement_calving[idx_row_calfdied48h_easy,idx_col_input_value]$input_value,
                                                      split = " "))[second_element_vector]
  }
  value2update_calvingdied24hprop_easy <- paste0(c(calvingdied24h_prop_prim_easy, rep(calvingdied24h_prop_multi_easy,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calfdied48h_easy,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calfdied48h_easy,idx_col_input],
                              ps_value2update = value2update_calvingdied24hprop_easy,
                              pb_log = pb_log,
                              plogger = lgr)
  calvingdied24h_prop_prim_difficult <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                           ps_statement_firstlactation = TRUE,
                                                                           ps_statement_easycalving = FALSE,
                                                                           pb_log = pb_log,
                                                                           plogger = lgr)
  calvingdied24h_prop_multi_difficult <- calculate_calvesdied24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                                            ps_statement_firstlactation = FALSE,
                                                                            ps_statement_easycalving = FALSE,
                                                                            pb_log = pb_log,
                                                                            plogger = lgr)
  # Check if calvingdied24h_prop_prim_difficult is zero. If it is the case, set a default value
  if(calvingdied24h_prop_prim_difficult == 0){
    calvingdied24h_prop_prim_difficult <- unlist(strsplit(tbl_input_statement_calving[idx_row_calfdied48h_dystocia,idx_col_input_value]$input_value,
                                                          split = " "))[first_element_vector]
  }

  # Check if calvingdied24h_prop_multi_difficult is zero. If it is the case, set a default value
  if(calvingdied24h_prop_multi_difficult == 0){
    calvingdied24h_prop_multi_difficult <- unlist(strsplit(tbl_input_statement_calving[idx_row_calfdied48h_dystocia,idx_col_input_value]$input_value,
                                                           split = " "))[second_element_vector]
  }
  value2update_calvingdied24hprop_difficult <- paste0(c(calvingdied24h_prop_prim_difficult, rep(calvingdied24h_prop_multi_difficult,9)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[idx_row_calfdied48h_dystocia,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[idx_row_calfdied48h_dystocia,idx_col_input],
                              ps_value2update = value2update_calvingdied24hprop_difficult,
                              pb_log = pb_log,
                              plogger = lgr)


  # Update Losses of calves from 48 hours after calving
  # Only required for beef-on-beef
  if(ps_prodsystew != l_constants_ew_input_beefOndairy$prodsyst4){
  calf_loss <- calculate_calvesdiedafter24h_proportion(ps_input_calving_tibble = tbl_calving,
                                                       pb_log = pb_log,
                                                       plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[l_constants_calving_beefOnbeef$idx_row_calfloss,l_constants_calving_beefOnbeef$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_calving[l_constants_calving_beefOnbeef$idx_row_calfloss,l_constants_calving_beefOnbeef$idx_col_input],
                             ps_value2update = calf_loss,
                             pb_log = pb_log,
                             plogger = lgr)


  # Update proportion of cows artificially inseminated in first oestrus
  dystocia_AI <- paste0(c(rep(1,10)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[l_constants_calving_beefOnbeef$idx_row_AI_dystocia,l_constants_calving_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[l_constants_calving_beefOnbeef$idx_row_AI_dystocia,l_constants_calving_beefOnbeef$idx_col_input],
                              ps_value2update = dystocia_AI,
                              pb_log = pb_log,
                              plogger = lgr)


  easy_AI <- paste0(c(rep(1,10)),collapse = " ")
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_calving[l_constants_calving_beefOnbeef$idx_row_AI_nodystocia,l_constants_calving_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_calving[l_constants_calving_beefOnbeef$idx_row_AI_nodystocia,l_constants_calving_beefOnbeef$idx_col_input],
                              ps_value2update = easy_AI,
                              pb_log = pb_log,
                              plogger = lgr)


  }
}


#' @title Pre-processing the progeny data flp for beef-on-beef input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on progeny flp data.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_progeny_flp_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the slaughter date to filter in the slaughter data
#' @param ps_end_flp_date setting the end of the slaughter date to filter in the slaughter data
#' @param ps_input_file_ped path to file with pedigree input
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#'
#' @export pre_process_ewbc_input_progeny_data_flp
pre_process_ewbc_input_progeny_data_flp <- function(ps_sirebreed,
                                                    ps_dambreed,
                                                    ps_prodsystew,
                                                    ps_marketchannel,
                                                    ps_path_directory2create,
                                                    ps_input_file_progeny_flp_statement,
                                                    ps_input_file_flp,
                                                    ps_start_flp_date,
                                                    ps_end_flp_date,
                                                    ps_input_file_ped,
                                                    pb_log,
                                                    plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ew_input_progeny_data_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ew_input_progeny_data_flp',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_progeny_flp_statement: ',ps_input_file_progeny_flp_statement, '\n',
                           ' * ps_input_file_flp: ', ps_input_file_flp, '\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n',
                           ' * ps_input_file_ped: ',ps_input_file_ped))
  }


  ### # Read file with statement based on progeny data flp for input-parameter-file of ECOWEIGHT
  tbl_input_statement_flp <- read_file_input(ps_input_file = ps_input_file_progeny_flp_statement,
                                             pb_log = pb_log,
                                             plogger = lgr)


  ### # Read file with progeny-flp data
  tbl_flp <- read_file_input_flp(ps_input_file_flp,
                                 ps_start_flp_date,
                                 ps_end_flp_date,
                                 pb_log = pb_log,
                                 plogger = lgr)


  ### # Get the constants
  l_constants_progeny_beefOnbeef <- get_constants_progeny_beefOnbeef()


  ### # Read pedigree file
  tbl_ped <- read_file_input_ped(ps_input_file_ped,
                                 pb_log = pb_log,
                                 plogger = lgr)


  ### # Merge progeny-flp data and pedigree files
  tbl_merged_data <- tbl_flp %>% dplyr::inner_join(tbl_ped, by = c("NakoTVD" = "TVDid"))
  ### # Create different tibble if purebred or crossbred
  ### # purebred beef
  if(ps_sirebreed == ps_dambreed){
    tbl_progeny_data_flp <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>% dplyr::filter(Mutter_RasseCode == ps_sirebreed)
  }else{
    ### # crossbred beef
    tbl_progeny_data_flp <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>% dplyr::filter(Mutter_RasseCode == ps_dambreed)
  }


  # ****************************************************************************
  ## ---- Natura-Beef ----
  # ****************************************************************************
  if(ps_marketchannel == "Natura-Beef"){
    # Update statement-progeny-flp-input from the data by calculating mean birth weight
    female_bw <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                            ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                            ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                            ps_prodsystew = ps_prodsystew,
                                            pb_log = pb_log,
                                            plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = female_bw,
                                pb_log = pb_log,
                                plogger = lgr)

    male_bw <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                          ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                          ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                          ps_prodsystew = ps_prodsystew,
                                          pb_log = pb_log,
                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = male_bw,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean live weight at slaughter
    livewt_slaughter_f <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                              ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                                              ps_prodsystew = ps_prodsystew,
                                                              pb_log = pb_log,
                                                              plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_heifer_slaughter,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_heifer_slaughter,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = livewt_slaughter_f,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_m <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                              ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                                              ps_prodsystew = ps_prodsystew,
                                                              pb_log = pb_log,
                                                              plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_bull_slaughter,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_bull_slaughter,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = livewt_slaughter_m,
                                pb_log = pb_log,
                                plogger = lgr)


    # Calculate weaning weight, weaning age, slaughter age
    weaningwt_f <- calculate_mean_weaningweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                                ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                                ps_prodsystew = ps_prodsystew,
                                                pb_log = pb_log,
                                                plogger = lgr)
    weaningwt_m <- calculate_mean_weaningweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                                ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                                ps_prodsystew = ps_prodsystew,
                                                pb_log = pb_log,
                                                plogger = lgr)
    weaningage_f <- calculate_mean_weaningage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                              ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                              ps_prodsystew = ps_prodsystew,
                                              pb_log = pb_log,
                                              plogger = lgr)
    weaningage_m <- calculate_mean_weaningage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                              ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                              ps_prodsystew = ps_prodsystew,
                                              pb_log = pb_log,
                                              plogger = lgr)
    slaughterage_f <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                  ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                                  ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                                  ps_prodsystew = ps_prodsystew,
                                                  pb_log = pb_log,
                                                  plogger = lgr)
    slaughterage_m <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                  ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                                  ps_marketing_channel = l_constants_progeny_beefOnbeef$Natura_Beef,
                                                  ps_prodsystew = ps_prodsystew,
                                                  pb_log = pb_log,
                                                  plogger = lgr)


    # Calculate daily gain
    dailygain_f <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f,
                                       pv_mean_weaningage = weaningage_f,
                                       pv_mean_livewt_atslaughter = livewt_slaughter_f,
                                       pv_mean_weaningwt = weaningwt_f,
                                       pb_log = pb_log,
                                       plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_heifer,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_heifer,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = dailygain_f,
                                pb_log = pb_log,
                                plogger = lgr)
    dailygain_m <- calculate_dailygain(pv_mean_slaughterage = slaughterage_m,
                                       pv_mean_weaningage = weaningage_m,
                                       pv_mean_livewt_atslaughter = livewt_slaughter_m,
                                       pv_mean_weaningwt = weaningwt_m,
                                       pb_log = pb_log,
                                       plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_bull,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_bull,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = dailygain_m,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating extrapolate weaning weight
    # extrapolated to 300 days
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_1weighing,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_1weighing,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = l_constants_progeny_beefOnbeef$extrapol_300d,
                                pb_log = pb_log,
                                plogger = lgr)
    weight_300d_f <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                          pv_daily_gain = dailygain_f,
                                                          pv_mean_weaningwt = weaningwt_f,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_300d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_300d_f,
                                pb_log = pb_log,
                                plogger = lgr)
    weight_300d_m <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                          pv_daily_gain = dailygain_m,
                                                          pv_mean_weaningwt = weaningwt_m,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_300d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_300d_m,
                                pb_log = pb_log,
                                plogger = lgr)


    # extrapolated to 302 days
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_2weighing,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_2weighing,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = l_constants_progeny_beefOnbeef$extrapol_302d,
                                pb_log = pb_log,
                                plogger = lgr)
    # extrapolated to 304 days
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_3weighing,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_3weighing,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = l_constants_progeny_beefOnbeef$extrapol_304d,
                                pb_log = pb_log,
                                plogger = lgr)
    # weaning weight at 302 days
    weight_302d_f <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                          pv_daily_gain = dailygain_f,
                                                          pv_mean_weaningwt = weaningwt_f,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_302d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_302d_f,
                                pb_log = pb_log,
                                plogger = lgr)
    weight_302d_m <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                          pv_daily_gain = dailygain_m,
                                                          pv_mean_weaningwt = weaningwt_m,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_302d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_302d_m,
                                pb_log = pb_log,
                                plogger = lgr)
    # weaning weight at 304 days
    weight_304d_f <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                          pv_daily_gain = dailygain_f,
                                                          pv_mean_weaningwt = weaningwt_f,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_304d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_304d_f,
                                pb_log = pb_log,
                                plogger = lgr)
    weight_304d_m <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                          pv_daily_gain = dailygain_m,
                                                          pv_mean_weaningwt = weaningwt_m,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_304d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_304d_m,
                                pb_log = pb_log,
                                plogger = lgr)
  }


  # ****************************************************************************
  ## ---- SwissPrimBeef ----
  # ****************************************************************************
  if(ps_marketchannel == "SwissPrimBeef"){

    # Update statement-progeny-flp-input from the data by calculating mean birth weight
    female_bw <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                            ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                            ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                            ps_prodsystew = ps_prodsystew,
                                            pb_log = pb_log,
                                            plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = female_bw,
                                pb_log = pb_log,
                                plogger = lgr)

    male_bw <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                          ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                          ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                          ps_prodsystew = ps_prodsystew,
                                          pb_log = pb_log,
                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bw_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = male_bw,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean weaning weight
    weaningwt_f <- calculate_mean_weaningweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                                ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                                ps_prodsystew = ps_prodsystew,
                                                pb_log = pb_log,
                                                plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                          paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                          tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weaningwt_f,
                                pb_log = pb_log,
                                plogger = lgr)
    weaningwt_m <- calculate_mean_weaningweight(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                                ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                                ps_prodsystew = ps_prodsystew,
                                                pb_log = pb_log,
                                                plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_1weighing_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weaningwt_m,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean weaning age
    weaningage_f <- calculate_mean_weaningage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                              ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                              ps_prodsystew = ps_prodsystew,
                                              pb_log = pb_log,
                                              plogger = lgr)
    weaningage_m <- calculate_mean_weaningage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                              ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                              ps_prodsystew = ps_prodsystew,
                                              pb_log = pb_log,
                                              plogger = lgr)
    weaningage_average <- round(as.numeric(weaningage_f+weaningage_m)/2,4)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_1weighing,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_1weighing,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weaningage_average,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean live weight at slaughter
    livewt_slaughter_f <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                              ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                                              ps_prodsystew = ps_prodsystew,
                                                              pb_log = pb_log,
                                                              plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_heifer_slaughter,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_heifer_slaughter,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = livewt_slaughter_f,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_m <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                              ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                                              ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                                              ps_prodsystew = ps_prodsystew,
                                                              pb_log = pb_log,
                                                              plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_bull_slaughter,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                 ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_lw_bull_slaughter,l_constants_progeny_beefOnbeef$idx_col_input],
                                 ps_value2update = livewt_slaughter_m,
                                 pb_log = pb_log,
                                 plogger = lgr)


    # Calculate slaughter age
    slaughterage_f <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                  ps_sex = l_constants_progeny_beefOnbeef$sex_female,
                                                  ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                                  ps_prodsystew = ps_prodsystew,
                                                  pb_log = pb_log,
                                                  plogger = lgr)
    slaughterage_m <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_progeny_data_flp,
                                                  ps_sex = l_constants_progeny_beefOnbeef$sex_male,
                                                  ps_marketing_channel = l_constants_progeny_beefOnbeef$SwissPrimBeef,
                                                  ps_prodsystew = ps_prodsystew,
                                                  pb_log = pb_log,
                                                  plogger = lgr)


    # Calculate and update daily gain
    dailygain_f <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f,
                                       pv_mean_weaningage = weaningage_f,
                                       pv_mean_livewt_atslaughter = livewt_slaughter_f,
                                       pv_mean_weaningwt = weaningwt_f,
                                       pb_log = pb_log,
                                       plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_heifer,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_heifer,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = dailygain_f,
                                pb_log = pb_log,
                                plogger = lgr)
    dailygain_m <- calculate_dailygain(pv_mean_slaughterage = slaughterage_m,
                                       pv_mean_weaningage = weaningage_m,
                                       pv_mean_livewt_atslaughter = livewt_slaughter_m,
                                       pv_mean_weaningwt = weaningwt_m,
                                       pb_log = pb_log,
                                       plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_bull,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_dg_bull,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = dailygain_m,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating extrapolate weaning weight
    # extrapolated to 300 days
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_2weighing,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_2weighing,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = l_constants_progeny_beefOnbeef$extrapol_300d,
                                pb_log = pb_log,
                                plogger = lgr)
    # extrapolated to 400 days
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_3weighing,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_age_3weighing,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = l_constants_progeny_beefOnbeef$extrapol_400d,
                                pb_log = pb_log,
                                plogger = lgr)
    # weaning weight at 300 days
    weight_300d_f <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                          pv_daily_gain = dailygain_f,
                                                          pv_mean_weaningwt = weaningwt_f,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_300d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_female,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_300d_f,
                                pb_log = pb_log,
                                plogger = lgr)
    weight_300d_m <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                          pv_daily_gain = dailygain_m,
                                                          pv_mean_weaningwt = weaningwt_m,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_300d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                          paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                          tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_2weighing_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_300d_m,
                                pb_log = pb_log,
                                plogger = lgr)


    # weaning weight at 400 days
    weight_400d_f <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_f,
                                                          pv_daily_gain = dailygain_f,
                                                          pv_mean_weaningwt = weaningwt_f,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_400d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_female,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_female,l_constants_progeny_beefOnbeef$idx_col_input],
                               ps_value2update = weight_400d_f,
                               pb_log = pb_log,
                               plogger = lgr)
    weight_400d_m <- calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                          pv_daily_gain = dailygain_m,
                                                          pv_mean_weaningwt = weaningwt_m,
                                                          pv_t_days = l_constants_progeny_beefOnbeef$extrapol_400d,
                                                          pb_log = pb_log,
                                                          plogger = lgr)
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_male,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_3weighing_male,l_constants_progeny_beefOnbeef$idx_col_input],
                                ps_value2update = weight_400d_m,
                                pb_log = pb_log,
                                plogger = lgr)
  }



  # Update statement-progeny-flp-input from the data by calculating cow weight after second calving
  second_calving_wt <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                                ps_first_calvingweight = FALSE,
                                                ps_second_calvingweight = TRUE,
                                                ps_dambreed = ps_dambreed,
                                                pb_log = pb_log,
                                                plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_cowwt_2calving,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_cowwt_2calving,l_constants_progeny_beefOnbeef$idx_col_input],
                              ps_value2update = second_calving_wt,
                              pb_log = pb_log,
                              plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_wtcow_2calving,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_wtcow_2calving,l_constants_progeny_beefOnbeef$idx_col_input],
                              ps_value2update = second_calving_wt,
                              pb_log = pb_log,
                              plogger = lgr)


  # Update statement-progeny-flp-input from the data by calculating mature cow weight
  mature_weight_cow <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                                ps_first_calvingweight = FALSE,
                                                ps_second_calvingweight = FALSE,
                                                ps_dambreed = ps_dambreed,
                                                pb_log = pb_log,
                                                plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_cowwt_3calving,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_cowwt_3calving,l_constants_progeny_beefOnbeef$idx_col_input],
                              ps_value2update = mature_weight_cow,
                              pb_log = pb_log,
                              plogger = lgr)


  # Update statement-progeny-flp-input from the data by calculating mature bull weight
  bull_mature_weight <- calculate_bull_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                                  ps_sirebreed = ps_sirebreed,
                                                  pb_log = pb_log,
                                                  plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bullwt_mature,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_bullwt_mature,l_constants_progeny_beefOnbeef$idx_col_input],
                              ps_value2update = bull_mature_weight,
                              pb_log = pb_log,
                              plogger = lgr)


  # Update statement-progeny-flp-input from the data by calculating cow weight after 1st calving
  first_calving_wt <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                               ps_first_calvingweight = TRUE,
                                               ps_second_calvingweight = FALSE,
                                               ps_dambreed = ps_dambreed,
                                               pb_log = pb_log,
                                               plogger = lgr)
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_cowwt_1calving,l_constants_progeny_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOnbeef$idx_row_cowwt_1calving,l_constants_progeny_beefOnbeef$idx_col_input],
                              ps_value2update = first_calving_wt,
                              pb_log = pb_log,
                              plogger = lgr)


}


#' @title Pre-processing the progeny data flp for beef-on-dairy input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on progeny flp data.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_progeny_flp_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the slaughter date to filter in the slaughter data
#' @param ps_end_flp_date setting the end of the slaughter date to filter in the slaughter data
#' @param ps_input_file_ped path to file with pedigree input
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#'
#' @export pre_process_ewdc_input_progeny_data_flp
pre_process_ewdc_input_progeny_data_flp <- function(ps_sirebreed,
                                                    ps_dambreed,
                                                    ps_prodsystew,
                                                    ps_marketchannel,
                                                    ps_path_directory2create,
                                                    ps_input_file_progeny_flp_statement,
                                                    ps_input_file_flp,
                                                    ps_start_flp_date,
                                                    ps_end_flp_date,
                                                    ps_input_file_ped,
                                                    pb_log,
                                                    plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ewdc_input_progeny_data_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ewdc_input_progeny_data_flp',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_progeny_flp_statement: ',ps_input_file_progeny_flp_statement, '\n',
                           ' * ps_input_file_flp: ', ps_input_file_flp, '\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n',
                           ' * ps_input_file_ped: ',ps_input_file_ped))
  }


  ### # Get the constants
  l_constants_progeny_beefOndairy <- get_constants_progeny_beefOndairy()


  ### # Read file with statement based on progeny data flp for input-parameter-file of ECOWEIGHT
  tbl_input_statement_flp <- read_file_input(ps_input_file = ps_input_file_progeny_flp_statement,
                                             pb_log = pb_log,
                                             plogger = lgr)


  ### # Read file with progeny-flp data
  tbl_flp <- read_file_input_flp(ps_input_file_flp = ps_input_file_flp,
                                 ps_start_flp_date = ps_start_flp_date,
                                 ps_end_flp_date = ps_end_flp_date,
                                 pb_log = pb_log,
                                 plogger = lgr)


  ### # Read pedigree file
  tbl_ped <- read_file_input_ped(ps_input_file_ped = ps_input_file_ped,
                                 pb_log = pb_log,
                                 plogger = lgr)


  ### # Merge progeny-flp data and pedigree files
  tbl_merged_data <- tbl_flp %>% dplyr::inner_join(tbl_ped, by = c("NakoTVD" = "TVDid"))
  ### # Create different tibble if purebred or crossbred
  tbl_purebred_beef <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>% dplyr::filter(Mutter_RasseCode == ps_sirebreed)
  tbl_purebred_dairy <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_dambreed) %>% dplyr::filter(Mutter_RasseCode == ps_dambreed)
  tbl_crossbred <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>% dplyr::filter(Mutter_RasseCode == ps_dambreed)


  # ****************************************************************************
  ## ---- Conventional-Beef ----
  # ****************************************************************************
  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_beef){

    # Update statement-progeny-flp-input from the data by calculating mean birth weight
    ## females
    female_bw_puredairy <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_purebred_dairy,
                                                      ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                      ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                      ps_prodsystew = ps_prodsystew,
                                                      pb_log = pb_log,
                                                      plogger = lgr)

    female_bw_cross <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_crossbred,
                                                  ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                  ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                  ps_prodsystew = ps_prodsystew,
                                                  pb_log = pb_log,
                                                  plogger = lgr)
    ### update parameter for female birth weight (1st value purebred calves, 2nd value crossbred calves)
    value2update_bw_female <- paste0(c(female_bw_puredairy, female_bw_cross),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_female,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_female,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_bw_female,
                                pb_log = pb_log,
                                plogger = lgr)

    ## males
    male_bw_puredairy <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_purebred_dairy,
                                                    ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                    ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                    ps_prodsystew = ps_prodsystew,
                                                    pb_log = pb_log,
                                                    plogger = lgr)
    male_bw_cross <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_crossbred,
                                                ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                ps_prodsystew = ps_prodsystew,
                                                pb_log = pb_log,
                                                plogger = lgr)

    ### update parameter for male birth weight (1st value purebred calves, 2nd value crossbred calves)
    value2update_bw_male <- paste0(c(male_bw_puredairy, male_bw_cross),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_male,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_male,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_bw_male,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean live weight at slaughter
    livewt_slaughter_f_purebeef <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_beef,
                                                                       ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                                       ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                                       ps_prodsystew = ps_prodsystew,
                                                                       pb_log = pb_log,
                                                                       plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = livewt_slaughter_f_purebeef,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_m_purebeef <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_beef,
                                                                       ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                                       ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                                       ps_prodsystew = ps_prodsystew,
                                                                       pb_log = pb_log,
                                                                       plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = livewt_slaughter_m_purebeef,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_f_puredairy <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_dairy,
                                                                        ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                                        ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                                        ps_prodsystew = ps_prodsystew,
                                                                        pb_log = pb_log,
                                                                        plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = livewt_slaughter_f_puredairy,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_m_puredairy <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_dairy,
                                                                        ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                                        ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                                        ps_prodsystew = ps_prodsystew,
                                                                        pb_log = pb_log,
                                                                        plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input],
                               ps_value2update = livewt_slaughter_m_puredairy,
                               pb_log = pb_log,
                               plogger = lgr)

    #crossbred slaughter weights required for claculation of daily gain
    livewt_slaughter_f_cross <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_crossbred,
                                                                        ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                                        ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                                        ps_prodsystew = ps_prodsystew,
                                                                        pb_log = pb_log,
                                                                        plogger = lgr)
    livewt_slaughter_m_cross <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_crossbred,
                                                                    ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                                    ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                                    ps_prodsystew = ps_prodsystew,
                                                                    pb_log = pb_log,
                                                                    plogger = lgr)


    #Daily gain birth to end of rearing (at approx. 90 days and 130kg according to Agroscope green book)
    age_1stfeeding <- l_constants_progeny_beefOndairy$age_1stfeeding
    value2update_age1stfeeding <- paste0(c(age_1stfeeding, age_1stfeeding),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_age1stfeeding,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_age1stfeeding,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_age1stfeeding,
                                pb_log = pb_log,
                                plogger = lgr)

    rearing_end_age <- l_constants_progeny_beefOndairy$rearing_end_age
    value2update_rearing_end_age <- paste0(c(rearing_end_age, rearing_end_age),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingage,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingage,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_rearing_end_age,
                                pb_log = pb_log,
                                plogger = lgr)

    #rearing weight only needed for daily gain calculations
    rearing_end_wt <- l_constants_progeny_beefOndairy$rearing_end_wt

    #daily gain from birth to rearing at 90 days
    dailygain_rearing_f_purebred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                            pv_mean_rearing_wt = rearing_end_wt,
                                                                            pv_mean_birthwt = female_bw_puredairy,
                                                                            pb_log = pb_log,
                                                                            plogger = lgr)

    dailygain_rearing_f_crossbred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                 pv_mean_rearing_wt = rearing_end_wt,
                                                                 pv_mean_birthwt = female_bw_cross,
                                                                 pb_log = pb_log,
                                                                 plogger = lgr)

    value2update_rearingf <- paste0(c(dailygain_rearing_f_purebred, dailygain_rearing_f_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_rearingf,
                                pb_log = pb_log,
                                plogger = lgr)

    dailygain_rearing_m_purebred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                            pv_mean_rearing_wt = rearing_end_wt,
                                                                            pv_mean_birthwt = male_bw_puredairy,
                                                                            pb_log = pb_log,
                                                                            plogger = lgr)


    dailygain_rearing_m_crossbred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                             pv_mean_rearing_wt = rearing_end_wt,
                                                                             pv_mean_birthwt = male_bw_cross,
                                                                             pb_log = pb_log,
                                                                             plogger = lgr)

    value2update_rearingm <- paste0(c(dailygain_rearing_m_purebred, dailygain_rearing_m_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_m,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_m,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_rearingm,
                                pb_log = pb_log,
                                plogger = lgr)

    #slaughter age required for calculation of daily gain during fattening
    slaughterage_f_purebeef <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_beef,
                                                           ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                           ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                           ps_prodsystew = ps_prodsystew,
                                                           pb_log = pb_log,
                                                           plogger = lgr)
    slaughterage_m_purebeef <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_beef,
                                                           ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                           ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                           ps_prodsystew = ps_prodsystew,
                                                           pb_log = pb_log,
                                                           plogger = lgr)
    slaughterage_f_puredairy <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_dairy,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    slaughterage_m_puredairy <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_dairy,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    slaughterage_f_crossbred <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_crossbred,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    slaughterage_m_crossbred <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_crossbred,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_beef,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)

    #daily gain during fattening
    dailygain_fattening_f_purebred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f_puredairy,
                                               pv_mean_weaningage = rearing_end_age,
                                               pv_mean_livewt_atslaughter = livewt_slaughter_f_puredairy,
                                               pv_mean_weaningwt = rearing_end_wt,
                                               pb_log = pb_log,
                                               plogger = lgr)

    dailygain_fattening_f_crossbred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f_crossbred,
                                                           pv_mean_weaningage = rearing_end_age,
                                                           pv_mean_livewt_atslaughter = livewt_slaughter_f_cross,
                                                           pv_mean_weaningwt = rearing_end_wt,
                                                           pb_log = pb_log,
                                                           plogger = lgr)

    value2update_fatteningf <- paste0(c(dailygain_fattening_f_purebred, dailygain_fattening_f_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_fatteningf,
                                pb_log = pb_log,
                                plogger = lgr)

    dailygain_fattening_m_purebred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_m_puredairy,
                                                 pv_mean_weaningage = rearing_end_age,
                                                 pv_mean_livewt_atslaughter = livewt_slaughter_m_puredairy,
                                                 pv_mean_weaningwt = rearing_end_wt,
                                                 pb_log = pb_log,
                                                 plogger = lgr)

    dailygain_fattening_m_crossbred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_m_crossbred,
                                                  pv_mean_weaningage = rearing_end_age,
                                                  pv_mean_livewt_atslaughter = livewt_slaughter_m_cross,
                                                  pv_mean_weaningwt = rearing_end_wt,
                                                  pb_log = pb_log,
                                                  plogger = lgr)



  }

  # ****************************************************************************
  ## ---- Conventional-Veal ----
  # ****************************************************************************
  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_calf){

    # Update statement-progeny-flp-input from the data by calculating mean birth weight
    female_bw_puredairy <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_purebred_dairy,
                                                      ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                      ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                      ps_prodsystew = ps_prodsystew,
                                                      pb_log = pb_log,
                                                      plogger = lgr)
    female_bw_cross <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_crossbred,
                                                  ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                  ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                  ps_prodsystew = ps_prodsystew,
                                                  pb_log = pb_log,
                                                  plogger = lgr)

    ### update parameter for female birth weight (1st value purebred calves, 2nd value crossbred calves)
    value2update_bw_female <- paste0(c(female_bw_puredairy, female_bw_cross),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_female,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_female,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_bw_female,
                                pb_log = pb_log,
                                plogger = lgr)


    male_bw_puredairy <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_purebred_dairy,
                                                    ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                    ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                    ps_prodsystew = ps_prodsystew,
                                                    pb_log = pb_log,
                                                    plogger = lgr)
    male_bw_cross <- calculate_mean_birthweight(ps_input_flp_tibble = tbl_crossbred,
                                                ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                ps_prodsystew = ps_prodsystew,
                                                pb_log = pb_log,
                                                plogger = lgr)

    value2update_bw_male <- paste0(c(male_bw_puredairy, male_bw_cross),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_male,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_bw_male,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_bw_male,
                                pb_log = pb_log,
                                plogger = lgr)


    # Update statement-progeny-flp-input from the data by calculating mean live weight at slaughter
    livewt_slaughter_f_purebeef <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_beef,
                                                                       ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                                       ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                                       ps_prodsystew = ps_prodsystew,
                                                                       pb_log = pb_log,
                                                                       plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = livewt_slaughter_f_purebeef,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_m_purebeef <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_beef,
                                                                       ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                                       ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                                       ps_prodsystew = ps_prodsystew,
                                                                       pb_log = pb_log,
                                                                       plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_beef,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = livewt_slaughter_m_purebeef,
                                pb_log = pb_log,
                                plogger = lgr)

    livewt_slaughter_f_puredairy <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_dairy,
                                                                        ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                                        ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                                        ps_prodsystew = ps_prodsystew,
                                                                        pb_log = pb_log,
                                                                        plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_heifer_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input],
                               ps_value2update = livewt_slaughter_f_puredairy,
                               pb_log = pb_log,
                               plogger = lgr)

    livewt_slaughter_m_puredairy <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_purebred_dairy,
                                                                        ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                                        ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                                        ps_prodsystew = ps_prodsystew,
                                                                        pb_log = pb_log,
                                                                        plogger = lgr)

    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_lw_bull_slaughter_dairy,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = livewt_slaughter_m_puredairy,
                                pb_log = pb_log,
                                plogger = lgr)

    #crossbred slaughter weights required for claculation of daily gain
    livewt_slaughter_f_cross <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_crossbred,
                                                                    ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                                    ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                                    ps_prodsystew = ps_prodsystew,
                                                                    pb_log = pb_log,
                                                                    plogger = lgr)
    livewt_slaughter_m_cross <- calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_crossbred,
                                                                    ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                                    ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                                    ps_prodsystew = ps_prodsystew,
                                                                    pb_log = pb_log,
                                                                    plogger = lgr)


    #Daily gain birth to end of rearing (at approx. 90 days and 130kg according to Agroscope green book)
    age_1stfeeding <- l_constants_progeny_beefOndairy$age_1stfeeding
    value2update_age1stfeeding <- paste0(c(age_1stfeeding, age_1stfeeding),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_age1stfeeding,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_age1stfeeding,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_age1stfeeding,
                                pb_log = pb_log,
                                plogger = lgr)

    rearing_end_age <- l_constants_progeny_beefOndairy$sale_age
    value2update_rearing_end_age <- paste0(c(rearing_end_age, rearing_end_age),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingage,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingage,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_rearing_end_age,
                                pb_log = pb_log,
                                plogger = lgr)

    #rearing weight only needed for daily gain calculations
    rearing_end_wt <- l_constants_progeny_beefOndairy$sale_wt

    #daily gain from birth to rearing at 90 days
    dailygain_rearing_f_purebred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                pv_mean_rearing_wt = rearing_end_wt,
                                                                pv_mean_birthwt = female_bw_puredairy,
                                                                pb_log = pb_log,
                                                                plogger = lgr)

    dailygain_rearing_f_crossbred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                 pv_mean_rearing_wt = rearing_end_wt,
                                                                 pv_mean_birthwt = female_bw_cross,
                                                                 pb_log = pb_log,
                                                                 plogger = lgr)

    value2update_rearingf <- paste0(c(dailygain_rearing_f_purebred, dailygain_rearing_f_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_rearingf,
                                pb_log = pb_log,
                                plogger = lgr)

    dailygain_rearing_m_purebred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                pv_mean_rearing_wt = rearing_end_wt,
                                                                pv_mean_birthwt = male_bw_puredairy,
                                                                pb_log = pb_log,
                                                                plogger = lgr)


    dailygain_rearing_m_crossbred <- calculate_dailygain_rearing(pv_mean_rearingage = rearing_end_age,
                                                                 pv_mean_rearing_wt = rearing_end_wt,
                                                                 pv_mean_birthwt = male_bw_cross,
                                                                 pb_log = pb_log,
                                                                 plogger = lgr)

    value2update_rearingm <- paste0(c(dailygain_rearing_m_purebred, dailygain_rearing_m_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_m,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_m,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_rearingm,
                                pb_log = pb_log,
                                plogger = lgr)

    #slaughter age required for calculation of daily gain during fattening
    slaughterage_f_purebeef <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_beef,
                                                           ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                           ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                           ps_prodsystew = ps_prodsystew,
                                                           pb_log = pb_log,
                                                           plogger = lgr)
    slaughterage_m_purebeef <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_beef,
                                                           ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                           ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                           ps_prodsystew = ps_prodsystew,
                                                           pb_log = pb_log,
                                                           plogger = lgr)
    slaughterage_f_puredairy <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_dairy,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    slaughterage_m_puredairy <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_purebred_dairy,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    slaughterage_f_crossbred <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_crossbred,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_female,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)
    slaughterage_m_crossbred <- calculate_mean_slaughterage(ps_input_flp_tibble = tbl_crossbred,
                                                            ps_sex = l_constants_progeny_beefOndairy$sex_male,
                                                            ps_marketing_channel = l_constants_progeny_beefOndairy$conv_fattening_calf,
                                                            ps_prodsystew = ps_prodsystew,
                                                            pb_log = pb_log,
                                                            plogger = lgr)

    #daily gain during fattening
    dailygain_fattening_f_purebred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f_puredairy,
                                                          pv_mean_weaningage = rearing_end_age,
                                                          pv_mean_livewt_atslaughter = livewt_slaughter_f_puredairy,
                                                          pv_mean_weaningwt = rearing_end_wt,
                                                          pb_log = pb_log,
                                                          plogger = lgr)

    dailygain_fattening_f_crossbred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_f_crossbred,
                                                           pv_mean_weaningage = rearing_end_age,
                                                           pv_mean_livewt_atslaughter = livewt_slaughter_f_cross,
                                                           pv_mean_weaningwt = rearing_end_wt,
                                                           pb_log = pb_log,
                                                           plogger = lgr)

    value2update_fatteningf <- paste0(c(dailygain_fattening_f_purebred, dailygain_fattening_f_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_f,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_fatteningf,
                                pb_log = pb_log,
                                plogger = lgr)

    dailygain_fattening_m_purebred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_m_puredairy,
                                                          pv_mean_weaningage = rearing_end_age,
                                                          pv_mean_livewt_atslaughter = livewt_slaughter_m_puredairy,
                                                          pv_mean_weaningwt = rearing_end_wt,
                                                          pb_log = pb_log,
                                                          plogger = lgr)

    dailygain_fattening_m_crossbred <- calculate_dailygain(pv_mean_slaughterage = slaughterage_m_crossbred,
                                                           pv_mean_weaningage = rearing_end_age,
                                                           pv_mean_livewt_atslaughter = livewt_slaughter_m_cross,
                                                           pv_mean_weaningwt = rearing_end_wt,
                                                           pb_log = pb_log,
                                                           plogger = lgr)
    value2update_fatteningm <- paste0(c(dailygain_fattening_f_purebred, dailygain_fattening_m_crossbred),collapse = " ")
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_m,l_constants_progeny_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_rearingdailygain_m,l_constants_progeny_beefOndairy$idx_col_input],
                                ps_value2update = value2update_fatteningm,
                                pb_log = pb_log,
                                plogger = lgr)

  }


  # Update statement-progeny-flp-input from the data by calculating mature cow weight
  mature_weight_cow_dairy <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                                      ps_first_calvingweight = FALSE,
                                                      ps_second_calvingweight = FALSE,
                                                      ps_dambreed = ps_dambreed,
                                                      pb_log = pb_log,
                                                      plogger = lgr)
  mature_weight_cow_beef <- calculate_cow_liveweight(ps_input_flp_tibble = tbl_merged_data,
                                                     ps_first_calvingweight = FALSE,
                                                     ps_second_calvingweight = FALSE,
                                                     ps_dambreed = ps_sirebreed,
                                                     pb_log = pb_log,
                                                     plogger = lgr)

#Not currently working in ewdc ecoweight (weight is too low)
  # update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
  #                                                                               paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
  #                                                                               tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_cowwt_dairy_3calving,l_constants_progeny_beefOndairy$idx_col_input_file]),
  #                             ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_cowwt_dairy_3calving,l_constants_progeny_beefOndairy$idx_col_input],
  #                             ps_value2update = mature_weight_cow_dairy,
  #                             pb_log = pb_log,
  #                             plogger = lgr)
  #
  # update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
  #                                                                               paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
  #                                                                               tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_cowwt_beef_3calving,l_constants_progeny_beefOndairy$idx_col_input_file]),
  #                             ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_cowwt_beef_3calving,l_constants_progeny_beefOndairy$idx_col_input],
  #                             ps_value2update = mature_weight_cow_beef,
  #                             pb_log = pb_log,
  #                             plogger = lgr)

#Age of calves sold for export
age_sale <- l_constants_progeny_beefOndairy$sale_age
value2update_agesale <- paste0(c(age_sale, age_sale),collapse = " ")
update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                              paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                              tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_agesale_f,l_constants_progeny_beefOndairy$idx_col_input_file]),
                            ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_agesale_f,l_constants_progeny_beefOndairy$idx_col_input],
                            ps_value2update = value2update_agesale,
                            pb_log = pb_log,
                            plogger = lgr)

update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                              paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                              tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_agesale_m,l_constants_progeny_beefOndairy$idx_col_input_file]),
                            ps_statement2search = tbl_input_statement_flp[l_constants_progeny_beefOndairy$idx_row_agesale_m,l_constants_progeny_beefOndairy$idx_col_input],
                            ps_value2update = value2update_agesale,
                            pb_log = pb_log,
                            plogger = lgr)



}


#' @title Pre-processing the carcass conformation, fat, prices based on flp-data for beef-on-beef input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on progeny flp data and literature about the price-system.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_flp_carcass_matrix_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the slaughter date to filter in the slaughter data
#' @param ps_end_flp_date setting the end of the slaughter date to filter in the slaughter data
#' @param ps_input_file_price_cow path to file with price for cows
#' @param ps_input_file_price_bull path to file with price for bulls
#' @param ps_input_file_price_heifer path tho file with price for heifers
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#'
#' @export pre_process_ewbc_input_carcass_data_flp
pre_process_ewbc_input_carcass_data_flp <- function(ps_sirebreed,
                                                  ps_dambreed,
                                                  ps_prodsystew,
                                                  ps_marketchannel,
                                                  ps_path_directory2create,
                                                  ps_input_file_flp_carcass_matrix_statement,
                                                  ps_input_file_flp,
                                                  ps_start_flp_date,
                                                  ps_input_file_ped,
                                                  ps_end_flp_date,
                                                  ps_input_file_price_cow,
                                                  ps_input_file_price_bull,
                                                  ps_input_file_price_heifer,
                                                  pb_log,
                                                  plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ewbc_input_carcass_data_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ewbc_input_carcass_data_flp',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                          ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_flp_carcass_matrix_statement: ',ps_input_file_flp_carcass_matrix_statement, '\n',
                           ' * ps_input_file_flp: ', ps_input_file_flp, '\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_input_file_ped: ',ps_input_file_ped,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n',
                           ' * ps_input_file_price_cow: ',ps_input_file_price_cow,'\n',
                           ' * ps_input_file_price_bull: ',ps_input_file_price_bull,'\n',
                           ' * ps_input_file_price_heifer: ',ps_input_file_price_heifer))
  }


  ### # Read file with statement based on carcass data flp for input-parameter-file of ECOWEIGHT
  tbl_input_statement_flp_carcass <- read_file_input(ps_input_file = ps_input_file_flp_carcass_matrix_statement,
                                                     pb_log = pb_log,
                                                     plogger = lgr)

  ### # Read file with progeny-flp data
  tbl_flp <- read_file_input_flp(ps_input_file_flp = ps_input_file_flp,
                                 ps_start_flp_date = ps_start_flp_date,
                                 ps_end_flp_date = ps_end_flp_date,
                                 pb_log = pb_log,
                                 plogger = lgr)

  tbl_ped <- read_file_input_ped(ps_input_file_ped = ps_input_file_ped,
                                 pb_log = pb_log,
                                 plogger = lgr)


  ### # Merge progeny-flp data and pedigree files
  tbl_merged_data <- tbl_flp %>% dplyr::inner_join(tbl_ped, by = c("NakoTVD" = "TVDid"))
  ### # Create different tibble if purebred or crossbred
  tbl_breed_data <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>% dplyr::filter(Mutter_RasseCode == ps_dambreed)

  ### # Get the constants
  l_constants_carcass_beefOnbeef <- get_constants_carcass_beefOnbeef()
  l_constants_carcass_beefOndairy <- get_constants_carcass_beefOndairy()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  l_constants_ew_input_beefOndairy <- get_constants_ew_input_beefOndairy()

  ### # Number of classes for fleshiness
    idx_row_class_fleshiness <- l_constants_carcass_beefOnbeef$idx_row_class_fleshiness
    idx_col_input_file <- l_constants_carcass_beefOnbeef$idx_col_input_file
    idx_col_input <- l_constants_carcass_beefOnbeef$idx_col_input
    idx_col_input_value <- l_constants_carcass_beefOnbeef$idx_col_input_value
    idx_row_class_fat <- l_constants_carcass_beefOnbeef$idx_row_class_fat

  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[idx_row_class_fleshiness,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[idx_row_class_fleshiness,idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[idx_row_class_fleshiness,idx_col_input_value]$input_value,
                              pb_log = pb_log,
                              plogger = lgr)


  ### # Number of classes for fat covering
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[idx_row_class_fat,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[idx_row_class_fat,idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[idx_row_class_fat,idx_col_input_value]$input_value,
                              pb_log = pb_log,
                              plogger = lgr)



  # ****************************************************************************
  ## ---- Cows frequencies ----
  # ****************************************************************************
  freq_mat_cow <- build_freq_conf_fat(ps_input_flp_tibble = tbl_breed_data,
                                      ps_sex = l_constants_carcass_beefOnbeef$sex_female,
                                      ps_marketing_channel = ps_marketchannel,
                                      ps_flag_cow = TRUE,
                                      pb_log = pb_log,
                                      plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOnbeef$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Heifers frequencies ----
  # ****************************************************************************
  freq_mat_heifer <- build_freq_conf_fat(ps_input_flp_tibble = tbl_breed_data,
                                         ps_sex = l_constants_carcass_beefOnbeef$sex_female,
                                         ps_marketing_channel = ps_marketchannel,
                                         ps_flag_cow = FALSE,
                                         pb_log = pb_log,
                                         plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOnbeef$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Bulls frequencies ----
  # ****************************************************************************
  freq_mat_bull <- build_freq_conf_fat(ps_input_flp_tibble = tbl_breed_data,
                                       ps_sex = l_constants_carcass_beefOnbeef$sex_male,
                                       ps_marketing_channel = ps_marketchannel,
                                       ps_flag_cow = FALSE,
                                       pb_log = pb_log,
                                       plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                             ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line2,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOnbeef$line2,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_freq_class,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOnbeef$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Prices ----
  # ****************************************************************************
  # Update statement_flp_carcass-input from the data
  # base price for cow
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_price,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_price,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_price,l_constants_carcass_beefOnbeef$idx_col_input_value]$input_value,
                              pb_log = pb_log,
                              plogger = lgr)
  # average price bull
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_avprice,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_avprice,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_avprice,l_constants_carcass_beefOnbeef$idx_col_input_value]$input_value,
                              pb_log = pb_log,
                              plogger = lgr)
  # basis price bull
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_price,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_price,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_price,l_constants_carcass_beefOnbeef$idx_col_input_value]$input_value,
                              pb_log = pb_log,
                              plogger = lgr)
  # basis price heifer
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_price,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_price,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_price,l_constants_carcass_beefOnbeef$idx_col_input_value]$input_value,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for cow----
  # ****************************************************************************
  mat_coeffprice_cow <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_cow,
                                            pb_log = pb_log,
                                            plogger = lgr)
  # Update price coefficient for cow
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                               ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line3,],4)),collapse = " "),
                               ps_line4statement2update = l_constants_carcass_beefOnbeef$line3,
                               pb_log = pb_log,
                               plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                               paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                               tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_cow_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                               ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOnbeef$line9,],4)),collapse = " "),
                               ps_line4statement2update = l_constants_carcass_beefOnbeef$line9,
                               pb_log = pb_log,
                               plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for bull----
  # ****************************************************************************
  mat_coeffprice_bull <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_bull,
                                             pb_log = pb_log,
                                             plogger = lgr)
  # Update price coefficient for bull
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                               ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line8,],4)),collapse = " "),
                               ps_line4statement2update = l_constants_carcass_beefOnbeef$line8,
                               pb_log = pb_log,
                               plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_bull_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOnbeef$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for heifer----
  # ****************************************************************************
  mat_coeffprice_heifer <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_heifer,
                                               pb_log = pb_log,
                                               plogger = lgr)
  # Update price coefficient for bull
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                               ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line1,],4)),collapse = " "),
                               pb_log = pb_log,
                               plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line2,
                              pb_log = pb_log,
                              plogger = lgr)
 ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                             ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line4,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOnbeef$line4,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                               ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line8,],4)),collapse = " "),
                               ps_line4statement2update = l_constants_carcass_beefOnbeef$line8,
                               pb_log = pb_log,
                               plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOnbeef$idx_row_heifer_coef,l_constants_carcass_beefOnbeef$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOnbeef$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOnbeef$line9,
                              pb_log = pb_log,
                              plogger = lgr)


}

#' @title Pre-processing the carcass conformation, fat, prices based on flp-data for beef-on-dairy input-parameter-file of ECOWEIGHT
#'
#' @description
#' The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
#' need input parameter files. This function processed different functions
#' to prepare the input parameter files based on progeny flp data and literature about the price-system.
#'
#' @param ps_sirebreed sire breed
#' @param ps_dambreed dam breed
#' @param ps_prodsystew production system build up as option in ECOWEIGHT
#' @param ps_marketchannel market channel
#' @param ps_path_directory2create path of the directory that will be created
#' @param ps_input_file_flp_carcass_matrix_statement path to file with statement based on calving for the input-parameter-file for ECOWEIGHT
#' @param ps_input_file_flp path to file with input coming from calving for the input-parameter-file for ECOWEIGHT
#' @param ps_start_flp_date setting the start of the slaughter date to filter in the slaughter data
#' @param ps_end_flp_date setting the end of the slaughter date to filter in the slaughter data
#' @param ps_input_file_price_cow path to file with price for cows
#' @param ps_input_file_price_bull path to file with price for bulls
#' @param ps_input_file_price_heifer path tho file with price for heifers
#' @param ps_input_file_price_calf path tho file with price for heifers
#' @param ps_input_file_ped input file for pedigree
#' @param pb_log indicator whether logs should be produced
#' @param plogger logger object
#'
#' @importFrom dplyr %>%
#' @import dplyr
#'
#' @export pre_process_ewdc_input_carcass_data_flp
pre_process_ewdc_input_carcass_data_flp <- function(ps_sirebreed,
                                                    ps_dambreed,
                                                    ps_prodsystew,
                                                    ps_marketchannel,
                                                    ps_path_directory2create,
                                                    ps_input_file_flp_carcass_matrix_statement,
                                                    ps_input_file_flp,
                                                    ps_input_file_ped,
                                                    ps_start_flp_date,
                                                    ps_end_flp_date,
                                                    ps_input_file_price_cow,
                                                    ps_input_file_price_bull,
                                                    ps_input_file_price_heifer,
                                                    ps_input_file_price_calf,
                                                    pb_log,
                                                    plogger = NULL){

  ### # Setting the log-file
  if(pb_log){
    if(is.null(plogger)){
      lgr <- get_qp4ewc_logger(ps_logfile = 'pre_process_ewdc_input_carcass_data_flp.log',
                               ps_level = 'INFO')
    }else{
      lgr <- plogger
    }
    qp4ewc_log_info(lgr, 'pre_process_ewdc_input_carcass_data_flp',
                    paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                           ' * ps_dambreed: ', ps_dambreed, '\n',
                           ' * ps_prodsystew: ', ps_prodsystew, '\n',
                           ' * ps_marketchannel: ', ps_marketchannel, '\n',
                           ' * ps_path_directory2create: ', ps_path_directory2create, '\n',
                           ' * ps_input_file_flp_carcass_matrix_statement: ',ps_input_file_flp_carcass_matrix_statement, '\n',
                           ' * ps_input_file_flp: ', ps_input_file_flp, '\n',
                           ' * ps_start_flp_date: ',ps_start_flp_date,'\n',
                           ' * ps_end_flp_date: ',ps_end_flp_date,'\n',
                           ' * ps_input_file_ped: ',ps_input_file_ped,'\n',
                           ' * ps_input_file_price_cow: ',ps_input_file_price_cow,'\n',
                           ' * ps_input_file_price_bull: ',ps_input_file_price_bull,'\n',
                           ' * ps_input_file_price_calf: ',ps_input_file_price_calf,'\n',
                           ' * ps_input_file_price_heifer: ',ps_input_file_price_heifer))
  }


  ### # Read file with statement based on carcass data flp for input-parameter-file of ECOWEIGHT
  tbl_input_statement_flp_carcass <- read_file_input(ps_input_file = ps_input_file_flp_carcass_matrix_statement,
                                                     pb_log = pb_log,
                                                     plogger = lgr)


  ### # Read file with progeny-flp data
  tbl_flp <- read_file_input_flp(ps_input_file_flp = ps_input_file_flp,
                                 ps_start_flp_date = ps_start_flp_date,
                                 ps_end_flp_date = ps_end_flp_date,
                                 pb_log = pb_log,
                                 plogger = lgr)

  tbl_ped <- read_file_input_ped(ps_input_file_ped,
                                 pb_log = pb_log,
                                 plogger = lgr)


  ### # Merge progeny-flp data and pedigree files
  tbl_merged_data <- tbl_flp %>% dplyr::inner_join(tbl_ped, by = c("NakoTVD" = "TVDid"))
  ### # Create different tibble if purebred or crossbred
  tbl_purebred_dairy_data <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_dambreed) %>% dplyr::filter(Mutter_RasseCode == ps_dambreed)
  tbl_crossbred_data <- tbl_merged_data %>% dplyr::filter(Vater_RasseCode == ps_sirebreed) %>% dplyr::filter(Mutter_RasseCode == ps_dambreed)

  ### # Get the constants
  l_constants_carcass_beefOnbeef <- get_constants_carcass_beefOnbeef()
  l_constants_carcass_beefOndairy <- get_constants_carcass_beefOndairy()
  l_constants_ewbc_input_beefOnbeef <- get_constants_ewbc_input_beefOnbeef()
  l_constants_ew_input_beefOndairy <- get_constants_ew_input_beefOndairy()
  l_constants_progeny_beefOndairy <- get_constants_progeny_beefOndairy()

  ### # Number of classes for fleshiness
    idx_row_class_fleshiness <- l_constants_carcass_beefOndairy$idx_row_class_fleshiness
    idx_col_input_file <- l_constants_carcass_beefOndairy$idx_col_input_file
    idx_col_input <- l_constants_carcass_beefOndairy$idx_col_input
    idx_col_input_value <- l_constants_carcass_beefOndairy$idx_col_input_value
    idx_row_class_fat <- l_constants_carcass_beefOndairy$idx_row_class_fat



  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[idx_row_class_fleshiness,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[idx_row_class_fleshiness,idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[idx_row_class_fleshiness,idx_col_input_value]$input_value_beef,
                              pb_log = pb_log,
                              plogger = lgr)


  ### # Number of classes for fat covering
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[idx_row_class_fat,idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[idx_row_class_fat,idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[idx_row_class_fat,idx_col_input_value]$input_value_beef,
                              pb_log = pb_log,
                              plogger = lgr)



  # ****************************************************************************
  ## ---- Cows frequencies ----
  # ****************************************************************************
  freq_mat_cow <- build_freq_conf_fat(ps_input_flp_tibble = tbl_purebred_dairy_data,
                                      ps_sex = l_constants_carcass_beefOndairy$sex_female,
                                      ps_marketing_channel = ps_marketchannel,
                                      ps_flag_cow = TRUE,
                                      pb_log = pb_log,
                                      plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                               ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                               pb_log = pb_log,
                               plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                               paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                               tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                             ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_cow[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Heifers frequencies purebred dairy ----
  # ****************************************************************************
  freq_mat_heifer_purebred <- build_freq_conf_fat(ps_input_flp_tibble = tbl_purebred_dairy_data,
                                                 ps_sex = l_constants_carcass_beefOndairy$sex_female,
                                                 ps_marketing_channel = ps_marketchannel,
                                                 ps_flag_cow = FALSE,
                                                 pb_log = pb_log,
                                                 plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer_purebred[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Bulls frequencies purebred dairy----
  # ****************************************************************************
  freq_mat_bull_purebred <- build_freq_conf_fat(ps_input_flp_tibble = tbl_purebred_dairy_data,
                                               ps_sex = l_constants_carcass_beefOndairy$sex_male,
                                               ps_marketing_channel = ps_marketchannel,
                                               ps_flag_cow = FALSE,
                                               pb_log = pb_log,
                                               plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                               paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                               tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                             ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class_purebred,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull_purebred[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)

  # ****************************************************************************
  ## ---- Heifers frequencies crossbred ----
  # ****************************************************************************
  freq_mat_heifer <- build_freq_conf_fat(ps_input_flp_tibble = tbl_crossbred_data,
                                         ps_sex = l_constants_carcass_beefOndairy$sex_female,
                                         ps_marketing_channel = ps_marketchannel,
                                         ps_flag_cow = FALSE,
                                         pb_log = pb_log,
                                         plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_heifer[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Bulls frequencies crossbred ----
  # ****************************************************************************
  freq_mat_bull <- build_freq_conf_fat(ps_input_flp_tibble = tbl_crossbred_data,
                                       ps_sex = l_constants_carcass_beefOndairy$sex_male,
                                       ps_marketing_channel = ps_marketchannel,
                                       ps_flag_cow = FALSE,
                                       pb_log = pb_log,
                                       plogger = lgr)


  # Update statement_flp_carcass-input from the data
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                             ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_freq_class,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(freq_mat_bull[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)




  # ****************************************************************************
  ## ---- Prices ----
  # ****************************************************************************
  # Update statement_flp_carcass-input from the data
  # base price for cow
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_price,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_price,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_price,l_constants_carcass_beefOndairy$idx_col_input_value]$input_value_beef,
                              pb_log = pb_log,
                              plogger = lgr)

  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_beef){
  # basis price bull
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_price,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_price,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_price,l_constants_carcass_beefOndairy$idx_col_input_value]$input_value_beef,
                              pb_log = pb_log,
                              plogger = lgr)
  # basis price heifer
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_price,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_price,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_price,l_constants_carcass_beefOndairy$idx_col_input_value]$input_value_beef,
                              pb_log = pb_log,
                              plogger = lgr)
  }
  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_calf){
    # basis price bull
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_price,l_constants_carcass_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_price,l_constants_carcass_beefOndairy$idx_col_input],
                                ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_price,l_constants_carcass_beefOndairy$idx_col_input_value_calf]$input_value_calf,
                                pb_log = pb_log,
                                plogger = lgr)
   # basis price heifer
    update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                  paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                  tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_price,l_constants_carcass_beefOndairy$idx_col_input_file]),
                                ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_price,l_constants_carcass_beefOndairy$idx_col_input],
                                ps_value2update = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_price,l_constants_carcass_beefOndairy$idx_col_input_value_calf]$input_value_calf,
                                pb_log = pb_log,
                                plogger = lgr)
  }


  # ****************************************************************************
  ## ---- Price coefficient for cow----
  # ****************************************************************************
  mat_coeffprice_cow <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_cow,
                                            pb_log = pb_log,
                                            plogger = lgr)
  # Update price coefficient for cow
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                             ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_cow_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_cow[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for bull----
  # ****************************************************************************
  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_beef){
  mat_coeffprice_bull <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_bull,
                                             pb_log = pb_log,
                                             plogger = lgr)
  }
  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_calf){
    mat_coeffprice_bull <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_calf,
                                               pb_log = pb_log,
                                               plogger = lgr)
  }

  # Update price coefficient for bull
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                               ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_bull_coef,l_constants_carcass_beefOndairy$idx_col_input],
                               ps_value2update = paste0(as.character(round(mat_coeffprice_bull[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                               ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                               pb_log = pb_log,
                               plogger = lgr)


  # ****************************************************************************
  ## ---- Price coefficient for heifer----
  # ****************************************************************************

  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_beef){
    mat_coeffprice_heifer <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_heifer,
                                                 pb_log = pb_log,
                                                 plogger = lgr)
  }
  if(ps_marketchannel == l_constants_progeny_beefOndairy$conv_fattening_calf){
    mat_coeffprice_heifer <- read_price_conf_fat(ps_input_file_price = ps_input_file_price_calf,
                                                 pb_log = pb_log,
                                                 plogger = lgr)
  }

  # Update price coefficient for bull
  ### # For carcass conformation C
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line1,],4)),collapse = " "),
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation H
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line2,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line2,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T+
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line3,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line3,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line4,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line4,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation T-
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line5,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line5,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation A
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                             ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                             ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line6,],4)),collapse = " "),
                             ps_line4statement2update = l_constants_carcass_beefOndairy$line6,
                             pb_log = pb_log,
                             plogger = lgr)
  ### # For carcass conformation X
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line7,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line7,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                               paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                               tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line8,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line8,
                              pb_log = pb_log,
                              plogger = lgr)
  ### # For carcass conformation XXX
  update_input_parameter_file(ps_path2template_input_parameter_file = file.path(ps_path_directory2create,
                                                                                paste0(ps_sirebreed,"_",ps_dambreed,"_",ps_prodsystew,"_",ps_marketchannel),
                                                                                tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input_file]),
                              ps_statement2search = tbl_input_statement_flp_carcass[l_constants_carcass_beefOndairy$idx_row_heifer_coef,l_constants_carcass_beefOndairy$idx_col_input],
                              ps_value2update = paste0(as.character(round(mat_coeffprice_heifer[l_constants_carcass_beefOndairy$line9,],4)),collapse = " "),
                              ps_line4statement2update = l_constants_carcass_beefOndairy$line9,
                              pb_log = pb_log,
                              plogger = lgr)


}

