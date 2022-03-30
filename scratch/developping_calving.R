### #
### #
### #
### #   Purpose:   Developping functions in RPackage qp4ewc
### #   started:   2022-03-04 (skn)
### #
### # ############################################## ###


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
