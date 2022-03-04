### #
### #
### #
### #   Purpose:   Developping functions in RPackage qp4ewc
### #   started:   2022-03-04 (skn)
### #
### # ############################################## ###


### # Developping logger-function get_qp4ewc_logger
ps_logfile = 'qp4ewc.log'
ps_level = 'INFO'
qp4ewc_logger <- log4r::create.logger(logfile = ps_logfile, level = ps_level)

### # Developping logger-function qp4ewc_log_info
plogger <- qp4ewc_logger
ps_caller <- 'qp4ewc_logger'
ps_msg <- paste0('Starting function qp4ewc_logger','\n')
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

### # Developping updating-function update_input_parameter_file
ps_path2template_input_parameter_file = "../../work/INPUT03.TXT"
ps_statement2search = "Amount of minerals per cow (including calf) and day"
ps_value2update = 0.25

plogger <- qp4ewc_logger
ps_caller <- 'update_input_parameter_file'
ps_msg <- paste0('Starting function with parameters:\n * ps_path2template_input_parameter_file: ', ps_path2template_input_parameter_file, '\n',
                 ' * ps_statement2search: ', ps_statement2search, '\n',
                 ' * ps_value2update: ', ps_value2update)
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

ps_msg <- paste0('File exists :\n * ps_path2template_input_parameter_file :',ps_path2template_input_parameter_file)
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

vec_ecow_input <- readLines(con = file(description = ps_path2template_input_parameter_file))

vec_linenb_statement2search <-grep(pattern = ps_statement2search , vec_ecow_input, fixed = TRUE)
ps_msg <- paste0('Surch:\n * ps_statement2search: ',ps_statement2search, '\n',
                 ' * Find statement on vector-line: ',vec_linenb_statement2search)
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

ps_value2update_idx <- grep(pattern = ps_statement2search , vec_ecow_input, fixed = TRUE) - 1
vec_ecow_input[ps_value2update_idx] <- ps_value2update
cat(paste0(vec_ecow_input, collapse = "\n"), "\n", file = ps_path2template_input_parameter_file, append = FALSE)
ps_msg <- paste0('Update the value:\n * ps_value2update: ',ps_value2update, '\n',
                 ' * of the statement ps_statement2search: ',ps_statement2search, '\n',
                 ' * on vector-line: ',ps_value2update_idx, '\n')
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

### # Developping input-function read_file_input_literature
ps_input_file_literature <- "inst/extdata/input_literature.txt"
ps_caller <- 'read_file_input_literature'
ps_msg <- paste0('File exists:\n * ps_input_file_literature',ps_input_file_literature)
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

tbl_input <- readr::read_delim(file = ps_input_file_literature, delim = ";")
ps_msg <- paste0('Read file:\n * ps_input_file_literature: ',ps_input_file_literature,"\n",
                 ' * in a tibble','\n')
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

### # Developping prepare_scenario-function create_directory_scenario
ps_sirebreed <- "LM"
ps_prodsystew <- "2"
ps_marketchannel <- "Natura-Beef"
ps_path_directory2create <- "/Users/skn/muku_Ecoweigth/2022/work/"

ps_msg <- paste0('Starting function with parameters:\n * ps_sirebreed: ', ps_sirebreed, '\n',
                 ' * ps_prodsystew: ', ps_prodsystew, '\n',
                 ' * ps_marketchannel: ', ps_marketchannel, '\n',
                 ' * ps_path_directory2create: ', ps_path_directory2create)
ps_caller <- 'create_directory_scenario'
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)

s_scenario <- paste0(ps_path_directory2create,ps_sirebreed,"_",ps_prodsystew,"_",ps_marketchannel)
ps_msg <- paste0('Define the name of the scenario:\n * s_scenario: ', s_scenario)
s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
log4r::info(logger = plogger, message = s_msg)


if(file.exists(s_scenario)){
  stop("The directory ",s_scenario," exists already!")
}else{
  dir.create(s_scenario)
  ps_msg <- paste0('Create a directory for scenario:\n * s_scenario: ', s_scenario)
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}

if(ps_prodsystew != "4"){
  list_of_files <- list.files("inst/templates/ewbc")
  file.copy(file.path('inst','templates','ewbc',list_of_files),s_scenario)
  ps_msg <- paste0('Copy the default input-parameter-files in the scenario directory based on the templates for ewbc')
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}


