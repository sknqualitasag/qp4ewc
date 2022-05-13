test_that("testing function update_input_parameter_file()", {

  # Read file with calving statement
  s_input_file_calving_statement <- file.path(here::here(),"inst","extdata","ewbc","input_calving_statement.txt")
  b_log <- TRUE
  tbl_input_statement_calving <- read_file_input(ps_input_file = s_input_file_calving_statement,
                                                 pb_log = b_log)

  # Get value for abortion rate that should be updated
  # Abortion rate for primiparous
  abortrate_prim <- 0.5
  # Abortion rate for multiparous
  abortrate_multi <- 0.3333
  value2update_abortrate <- paste0(c(abortrate_prim, rep(abortrate_multi,9)),collapse = " ")


  # Parameter for the function
  s_sirebreed <- "LM"
  s_prodsystew <- "2"
  s_marketingchannel <- "Natura-Beef"
  s_path_directory2create_test <- file.path(here::here(),"inst","extdata","ewbc","test")

  s_path2template_input_parameter_file = file.path(s_path_directory2create_test,paste0(s_sirebreed,"_",s_prodsystew,"_",s_marketingchannel),tbl_input_statement_calving[1,1])
  ### # Build up a vector with the input-parameter-file where each line of the input has a belonging number
  vec_ecow_input <- readLines(con = file(description = s_path2template_input_parameter_file))
  ### # Surch the statement in the input-parameter-file
  vec_linenb_statement2search <-grep(pattern = tbl_input_statement_calving[1,2] , vec_ecow_input, fixed = TRUE)
  ### # Update the value in the input-parameter-file
  ps_value2update_idx <- grep(pattern = tbl_input_statement_calving[1,2] , vec_ecow_input, fixed = TRUE) - 1


  expect_equal(vec_ecow_input[[ps_value2update_idx]],value2update_abortrate)

})
