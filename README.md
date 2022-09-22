
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qp4ewc

<!-- badges: start -->
<!-- badges: end -->

The goal of qp4ewc is to provide information that is required as input
for the program ECOWEIGHT in a uniform and easy to use way. The needed
input is read from different sources is processed and then converted
into the format that is required by ECOWEIGHT.

## Installation

You can install the development version of qp4ewc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sknqualitasag/qp4ewc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qp4ewc)
## basic example code
s_sirebreed <- "LM"
s_dambreed <- "LM"
s_prodsystem <- "1"
s_marketingchannel <- "Natura-Beef"
s_path_directory2create <- file.path(here::here(),"inst","extdata","ewbc","test","Preprocess_output")
s_input_file_literature <- system.file("extdata","ewbc","input_literature.txt", package = "qp4ewc")
s_input_file_par <- system.file("extdata","ewbc","input_par_statement.txt", package = "qp4ewc")
s_input_file_35 <- system.file("extdata","ewbc","input35_statement.txt", package = "qp4ewc")
s_input_file_36 <- system.file("extdata","ewbc","input36_statement.txt", package = "qp4ewc")
s_input_file_testedbulls <- system.file("extdata","ewbc","input_testedbulls.txt", package = "qp4ewc")
s_input_file_purchasedreplacementheifers  <- system.file("extdata","ewbc","input_purchasedreplacementheifers.txt", package = "qp4ewc")
s_input_file_calving_statement <- system.file("extdata","ewbc","input_calving_statement.txt", package = "qp4ewc")
s_input_file_calving <- system.file("extdata","ewbc","test","test_zws_muku_gal.csv", package = "qp4ewc")
s_start_date <- 20000101
s_end_date <- 20211231
s_input_file_progeny_flp_statement <- system.file("extdata","ewbc","input_flp_statement.txt", package = "qp4ewc")
s_input_file_flp <- system.file("extdata","ewbc","test","test_zws_muku_flp.csv", package = "qp4ewc")
s_input_file_calf <- system.file("extdata","ewdc","test","test_calf_data.csv", package = "qp4ewc")
s_input_file_flp_carcass_matrix_statement <- system.file("extdata","ewbc","input_flp_carcass_matrix_statement.txt", package = "qp4ewc")
s_input_file_price_cow <- system.file("extdata","ewbc","cow_price.txt", package = "qp4ewc")
s_input_file_price_bull <- system.file("extdata","ewbc","bull_price.txt", package = "qp4ewc")
s_input_file_price_heifer <- system.file("extdata","ewbc","heifer_price.txt", package = "qp4ewc")
s_input_file_price_calf <- system.file("extdata","ewdc","veal_price.txt", package = "qp4ewc")
s_input_file_ped <- system.file("extdata","ewbc","test","test_pedigree.txt", package = "qp4ewc")
b_log <- TRUE
pre_process_ew_input(ps_sirebreed = s_sirebreed,
                     ps_dambreed = s_dambreed,
                     ps_prodsystew = s_prodsystem,
                     ps_marketchannel = s_marketingchannel,
                     ps_path_directory2create = s_path_directory2create,
                     ps_input_file_literature = s_input_file_literature,
                     ps_input_file_par = s_input_file_par,
                     ps_input_file_35 = s_input_file_35,
                     ps_input_file_36 = s_input_file_36,
                     ps_input_file_testedbulls = s_input_file_testedbulls,
                     ps_input_file_purchasedreplacementheifers = s_input_file_purchasedreplacementheifers,
                     ps_input_file_calving_statement = s_input_file_calving_statement,
                     ps_input_file_calving = s_input_file_calving,
                     ps_start_date = s_start_date,
                     ps_end_date = s_end_date,
                     ps_input_file_progeny_flp_statement = s_input_file_progeny_flp_statement,
                     ps_input_file_flp = s_input_file_flp,
                     ps_input_file_calf = s_input_file_calf,
                     ps_input_file_flp_carcass_matrix_statement = s_input_file_flp_carcass_matrix_statement,
                     ps_input_file_price_cow = s_input_file_price_cow,
                     ps_input_file_price_bull = s_input_file_price_bull,
                     ps_input_file_price_heifer = s_input_file_price_heifer,
                     ps_input_file_price_calf = s_input_file_price_calf,
                     ps_input_file_ped = s_input_file_ped,
                     pb_log = b_log)
```