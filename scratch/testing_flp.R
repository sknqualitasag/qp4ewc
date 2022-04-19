### #
### #
### #
### #   Purpose:   Function related to the flp steps
### #   started:   2022-04-05 (skn)
### #
### # ##################################################################### ###

s_input_file_flp <- "/Users/skn/muku_Ecoweigth/2022/data/zws_muku_flp/test_ANAUCHLMSIOB_zws_muku_flp.csv"
s_start_flp_date <- 20160101
s_end_flp_date <- 20211231
s_log <- TRUE
s_sirebreed <- "LM"
s_sex <- "M"
s_marketing_channel = 2
s_lactationnumber = 2
s_input_file_progeny_flp_statement <- file.path(here::here(),"inst","extdata","input_flp_statement.txt")


tbl_flp <- qp4ewc::read_file_input_flp(ps_input_file_flp = s_input_file_flp,
                                       ps_start_flp_date = s_start_flp_date,
                                       ps_end_flp_date = s_end_flp_date,
                                       ps_sirebreed = s_sirebreed,
                                       pb_log = s_log,
                                       plogger = NULL)


tbl_input_statement_flp <- qp4ewc::read_file_input(ps_input_file = s_input_file_progeny_flp_statement,
                                                   pb_log = s_log,
                                                   plogger = NULL)



male_bw <- qp4ewc::calculate_mean_birthweight(ps_input_flp_tibble = tbl_flp,
                                              ps_sex = s_sex,
                                              ps_marketing_channel = s_marketing_channel,
                                              pb_log = s_log,
                                              plogger = NULL)


weaningwt_m <- qp4ewc::calculate_mean_weaningweight(ps_input_flp_tibble = tbl_flp,
                                                    ps_sex = s_sex,
                                                    ps_marketing_channel = s_marketing_channel,
                                                    pb_log = s_log,
                                                    plogger = NULL)


weaningage_m <- qp4ewc::calculate_mean_weaningage(ps_input_flp_tibble = tbl_flp,
                                                  ps_sex = s_sex,
                                                  ps_marketing_channel = s_marketing_channel,
                                                  pb_log = s_log,
                                                  plogger = NULL)


livewt_slaughter_m <- qp4ewc::calculate_mean_liveweight_slaughter(ps_input_flp_tibble = tbl_flp,
                                                                  ps_sex = s_sex,
                                                                  ps_marketing_channel = s_marketing_channel,
                                                                  pb_log = s_log,
                                                                  plogger = NULL)


slaughterage_m <- qp4ewc::calculate_mean_slaughterage(ps_input_flp_tibble = tbl_flp,
                                                      ps_sex = s_sex,
                                                      ps_marketing_channel = s_marketing_channel,
                                                      pb_log = s_log,
                                                      plogger = NULL)

dailygain_m <- qp4ewc::calculate_dailygain(pv_mean_slaughterage = slaughterage_m,
                                           pv_mean_weaningage = weaningage_m,
                                           pv_mean_livewt_atslaughter = livewt_slaughter_m,
                                           pv_mean_weaningwt = weaningwt_m,
                                           pb_log = s_log,
                                           plogger = NULL)


weight_300d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                              pv_daily_gain = dailygain_m,
                                                              pv_mean_weaningwt = weaningwt_m,
                                                              pv_t_days = 300,
                                                              pb_log = s_log,
                                                              plogger = NULL)


weight_302d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                              pv_daily_gain = dailygain_m,
                                                              pv_mean_weaningwt = weaningwt_m,
                                                              pv_t_days = 302,
                                                              pb_log = s_log,
                                                              plogger = NULL)


weight_304d_m <- qp4ewc::calculate_extrapolated_weaningweight(pv_mean_weaningage = weaningage_m,
                                                              pv_daily_gain = dailygain_m,
                                                              pv_mean_weaningwt = weaningwt_m,
                                                              pv_t_days = 304,
                                                              pb_log = s_log,
                                                              plogger = NULL)


second_calving_wt <- qp4ewc::calculate_cow_liveweight(ps_input_flp_tibble = tbl_flp,
                                                      ps_second_calvingweight = TRUE,
                                                      pb_log = s_log,
                                                      plogger = NULL)


mature_weight_cow <- qp4ewc::calculate_cow_liveweight(ps_input_flp_tibble = tbl_flp,
                                                      ps_second_calvingweight = FALSE,
                                                      pb_log = s_log,
                                                      plogger = NULL)


bull_mature_weight <- qp4ewc::calculate_bull_liveweight(ps_input_flp_tibble = tbl_flp,
                                                        pb_log = s_log,
                                                        plogger = NULL)
