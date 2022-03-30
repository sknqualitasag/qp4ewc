### #
### #
### #
### #   Purpose:   Developping functions in RPackage qp4ewc
### #   started:   2022-03-04 (skn)
### #
### # ############################################## ###

library(dplyr)

### # ############################################## ###
### # Input from calving data
### # ############################################## ###

### # Developping calving-function calculate_abortion_rate
ps_input_calving_tibble <- tbl_calving
ps_statement_firstlactation = FALSE

tbl_input <- ps_input_calving_tibble %>% filter(Geburtsverlauf != 0)

if(ps_statement_firstlactation){
  tbl_abort <- tbl_input %>% filter(Laktationsnummer_Mutter == 1) %>%
    select(Abort) %>%
    na.omit() %>%
    group_by(Abort) %>%
    count()
}else{
  tbl_abort <- tbl_input %>% filter(Laktationsnummer_Mutter > 1) %>%
    select(Abort) %>%
    na.omit() %>%
    group_by(Abort) %>%
    count()
}

if(nrow(tbl_abort %>% filter(Abort == 1)) != 0){
  print("continue")
}else{
  stop("calculate_abortion_rate: no abort information are available in the dataset, please check the dataset !")
}

abort_freq <- tbl_abort %>% filter(Abort == 1) %>% pull(n)
sum_abort_freq <- sum(tbl_abort$n)

abortion_rate <- round(abort_freq/sum_abort_freq,4)

### # Developping calving-function calculate_stillbirth_rate
ps_input_calving_tibble <- tbl_calving
ps_statement_firstlactation <- TRUE
ps_statement_easycalving <- TRUE

tbl_input <- ps_input_calving_tibble %>% dplyr::filter(Geburtsverlauf == 1 | Geburtsverlauf == 2)

tbl_stillbirth <- tbl_input %>% dplyr::filter(Laktationsnummer_Mutter == 1) %>%
  dplyr::select(Code_TotOLebend) %>%
  na.omit() %>%
  dplyr::group_by(Code_TotOLebend) %>%
  dplyr::count()

stillbirth_freq <- tbl_stillbirth %>% dplyr::filter(Code_TotOLebend == 4) %>% dplyr::pull(n)
sum_stillbirth_freq <- sum(tbl_stillbirth$n)

stillbirth_rate <- round(stillbirth_freq/sum_stillbirth_freq,4)
