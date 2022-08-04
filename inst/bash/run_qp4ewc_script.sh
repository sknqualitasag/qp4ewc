#!/bin/bash
#' ---
#' title: run qp4ewc
#' date:  2022-05-13 14:50:59
#' author: Sophie Kunz
#' ---
#' ## Purpose
#'
#'
#' ## Description
#' Run functionalities of the RPackage qp4ewc
#'
#' ## Details
#' Run functionalities of the RPackage qp4ewc
#'
#' ## Example
#'
#'
#' ## Set Directives
#' General behavior of the script is driven by the following settings
#+ bash-env-setting, eval=FALSE
set -o errexit    # exit immediately, if single command exits with non-zero status
set -o nounset    # treat unset variables as errors
set -o pipefail   # return value of pipeline is value of last command to exit with non-zero status
                  # hence pipe fails if one command in pipe fails

#' ## Global Constants
#' This section stores the directory of this script, the name of this script and the
#' hostname in a variable. Both variables are important for logfiles to be able to
#' trace back which output was produced by which script and on which server.
#+ script-files, eval=FALSE
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
SCRIPT=$(basename ${BASH_SOURCE[0]})
SERVER=`hostname`
INSTALLDIR=$(dirname ${BASH_SOURCE[0]})
PROJDIR=$(dirname $INSTALLDIR)
INPUTDIR=$PROJDIR/extdata



#' ## Functions
#' The following definitions of general purpose functions are local to this script.
#'
#' ### Start Message
#' The following function produces a start message showing the time
#' when the script started and on which server it was started.
#+ start-msg-fun, eval=FALSE
start_msg () {
    echo "********************************************************************************"
    echo "Starting $SCRIPT at: "`date +"%Y-%m-%d %H:%M:%S"`
    echo "Server:  $SERVER"
    echo ""
}

#' ### End Message
#' This function produces a message denoting the end of the script including
#' the time when the script ended. This is important to check whether a script
#' did run successfully to its end.
#+ end-msg-fun, eval=FALSE
end_msg () {
    echo ""
    echo "End of $SCRIPT at: "`date +"%Y-%m-%d %H:%M:%S"`
    echo "********************************************************************************"
}

#' ### Log Message
#' Log messages formatted similarly to log4r are produced.
#+ log-msg-fun, eval=FALSE
log_msg () {
  local l_CALLER=$1
  local l_MSG=$2
  local l_RIGHTNOW=`date +"%Y%m%d%H%M%S"`
  echo "[${l_RIGHTNOW} -- ${l_CALLER}] $l_MSG"
}

#' ### Usage Message
#' Usage message giving help on how to use the script.
#+ usage-msg-fun, eval=FALSE
usage () {
    local l_MSG=$1
    >&2 echo "Usage Error: $l_MSG"
    >&2 echo "Usage: $SCRIPT -a <a_example> -b <b_example> -c"
    >&2 echo "  where -a <a_example> ..."
    >&2 echo "        -b <b_example> (optional) ..."
    >&2 echo "        -c (optional) ..."
    >&2 echo ""
    exit 1
}


#' ## Parse and check command line arguments
#' Use getopts for commandline argument parsing
#' If an option should be followed by an argument, it should be followed by a ":".
#' Notice there is no ":" after "h". The leading ":" suppresses error messages from
#' getopts. This is required to get my unrecognized option code to work.
#+ getopts-parsing, eval=FALSE
OPTIND=1 # A POSIX variable. Reset in case getopts has been used previously in the shell.
par_file=""
while getopts ":p:h" opt; do
    case "$opt" in
        p)
            if test -f $OPTARG; then
                par_file=$OPTARG
            else
                >&2 echo "Error: $OPTARG isn't a regular file"
                usage
                exit 1
            fi
        ;;
        h)
            usage
            exit 0
        ;;
        :)
            >&2 echo "Error: -$OPTARG requires an argument"
            usage
            exit 1
        ;;
        ?)
            >&2 echo "Error: Invalid command line argument (-$OPTARG) found"
            usage
            exit 1
        ;;
    esac
done


#' ## Check whether required arguments have been specified
#+ argument-test, eval=FALSE
if test -z $par_file; then
    >&2 echo "Error: par_file is missing"
    usage
    exit 1
fi


#' assign evaluation directory and change dir to it
#+ assign-eval-dir, eval=FALSE
eval_dir=$(pwd | awk '{print $1}')
par_dir=$eval_dir/par
par_file_name=$(echo $par_file | rev | cut -d"/" -f1 | rev)
source $par_dir/$par_file_name  # Dadurch wird sichergestellt, dass man beim Programmstart am richtigen Ort ist
prog_dir=$eval_dir/prog
work_dir=$eval_dir/work/
mkdir -p ${work_dir}
cd ${work_dir}


#' Check if the mandatory files are available
#+ check-mandatory-files
if ! test -s ${s_input_file_calving}; then
  echo "FEHLER: calving data file is missing --> PROGRAMMABBRUCH'"
  exit 1
fi
if ! test -s ${s_input_file_flp}; then
  echo "FEHLER: carcass data file is missing --> PROGRAMMABBRUCH'"
  exit 1
fi
if ! test -s ${rrtdm_pedigree}; then
  echo "FEHLER: rrtdm-Pedigree is missing --> PROGRAMMABBRUCH'"
  exit 1
fi


#' Set variable of the run
#+ set-run-variable
Lauf_Bez=qp4ewc


#' ## Main Body of Script
#' The main body of the script starts here.
#+ start-msg, eval=FALSE
start_msg > $Lauf_Bez.runLog

#********************************************************************************************************************************************
#' ## Your Code
#' Continue to put your code here
echo "====================================================================================================" >> $Lauf_Bez.runLog
#' Filtering the calving data based on the date given in the parameter-file
#+ filtering_calving_data
echo "Start filtering calving data" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog
head -1 ${s_input_file_calving} > zws_muku_gal.csv
awk -F";" '{if($6>='${s_start_date}' && $6<='${s_end_date}') print $0}' ${s_input_file_calving} >> zws_muku_gal.csv
echo "End filtering calving data" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog


echo "====================================================================================================" >> $Lauf_Bez.runLog
#' Filtering the carcass data based on the date given in the parameter-file
#+ filtering_carcass_data
echo "Start filtering carcass data" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog
head -1 ${s_input_file_flp} > zws_muku_flp.csv
awk -F";" '{if($15>='${s_start_date}' && $15<='${s_end_date}') print $0}' ${s_input_file_flp} >> zws_muku_flp.csv
echo "End filtering carcass data" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog


echo "====================================================================================================" >> $Lauf_Bez.runLog
#' Create a list with TVDid of calf and slaugther animal based on the filtered data of calving and carcass
#+ create_list_TVDid
echo "Start building a list of TVDid" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog
echo "TVDid" > TVDids
sed '1d' zws_muku_flp.csv | awk -F";" '{print $1}' > tmp_id
sed '1d' zws_muku_gal.csv | awk -F";" '{print $7}' >> tmp_id
sort tmp_id | uniq -c | awk '{print $2}' >> TVDids
rm tmp_id
echo "End building a list of TVDid" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog


echo "====================================================================================================" >> $Lauf_Bez.runLog
#' Get breed code of sire, dam, sire of dam, dam of dam
#+ get_breedcode
echo "Start searching breed code of the TVDid list" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog
# Based on the TVDid get the pedigree info of the animal
awk '{if(FILENAME==ARGV[1]) {id[$1]} else {if(substr($0,58,14) in id) print $6" "substr($0,39,2)" "$1" "$2" "$3}}' TVDids ${rrtdm_pedigree} > tmp_animPed
# Get breed code of the sire
awk '{if(FILENAME==ARGV[1]) {id[$1]=substr($0,39,2)} else {if($4 in id) print $0" "id[$4]}}' ${rrtdm_pedigree} tmp_animPed > tmp_animPed_sire
# Get breed code of the dam
awk '{if(FILENAME==ARGV[1]) {id[$1]=substr($0,39,2)} else {if($5 in id) print $0" "id[$5]}}' ${rrtdm_pedigree} tmp_animPed_sire > tmp_animPed_sire_dam
# Transform the file after animid breed code
awk '{print $1" "$3" "$2" "$4" "$6" "$5" "$7}' tmp_animPed_sire_dam > tmp_animPed_sire_dam_transform
# Get the parent of the dam of the animal (calf or slaughter animal)
awk '{if(FILENAME==ARGV[1]) {id[$1]=$2" "$3} else {if($6 in id) print $0" "id[$6]}}' ${rrtdm_pedigree} tmp_animPed_sire_dam_transform > tmp_damPed
# Get breed code of the sire of the dam
awk '{if(FILENAME==ARGV[1]) {id[$1]=substr($0,39,2)} else {if($8 in id) print $0" "id[$8]}}' ${rrtdm_pedigree} tmp_damPed > tmp_damPed_sirebreed
# Get breed code of the dam of the dam
awk '{if(FILENAME==ARGV[1]) {id[$1]=substr($0,39,2)} else {if($9 in id) print $0" "id[$9]}}' ${rrtdm_pedigree} tmp_damPed_sirebreed > tmp_damPed_sirebreed_dambreed
# Transform the file
echo "TVDid animId animBreed sireId sireBreed damId damBreed sireofdamId sireofdamBreed damofdamId damofdamBreed" > ped_withBreedCode_damside
awk '{print $1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$10" "$9" "$11}' tmp_damPed_sirebreed_dambreed >> ped_withBreedCode_damside
# Delete the temporary files
rm tmp_animPed
rm tmp_animPed_sire
rm tmp_animPed_sire_dam
rm tmp_animPed_sire_dam_transform
rm tmp_damPed
rm tmp_damPed_sirebreed
rm tmp_damPed_sirebreed_dambreed
echo "End searching breed code of the TVDid list" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog


echo "====================================================================================================" >> $Lauf_Bez.runLog
#' Pre-processing the input-parameter-file of ECOWEIGHT beef cattle (ewbc)
#+ pre_process_ewbc_input
echo "Start qp4ewc::pre_process_ew_input() function for beef-on-beef (ewbc)" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog

actual_date=$(date +"%Y%m%d%H%M%S")

for s_sirebreed in AN AU CH LM OB SI;do
    for s_prodsystem in 1 3;do

     if [ "$s_prodsystem" == 1 ]; then
        s_dambreed=${s_sirebreed}

        echo "scenario: ${s_sirebreed}_${s_dambreed}_${s_prodsystem}_Natura-Beef " >> $Lauf_Bez.runLog
        # prepare ECOWEIGHT input-files for each scenario
        Rscript -e "qp4ewc::pre_process_ew_input(ps_sirebreed = '${s_sirebreed}',
                                         ps_dambreed = '${s_dambreed}',
                                         ps_prodsystew = '${s_prodsystem}',
                                         ps_marketchannel = 'Natura-Beef',
                                         ps_path_directory2create = '${s_path_directory2create}',
                                         ps_input_file_literature = '${s_input_file_literature_ewbc}',
                                         ps_input_file_par = '${s_input_file_par_ewbc}',
                                         ps_input_file_35 = '${s_input_file_35}',
                                         ps_input_file_36 = '${s_input_file_36}',
                                         ps_input_file_testedbulls = '${s_input_file_testedbulls}',
                                         ps_input_file_purchasedreplacementheifers = '${s_input_file_purchasedreplacementheifers}',
                                         ps_input_file_calving_statement = '${s_input_file_calving_statement_ewbc}',
                                         ps_input_file_calving = 'zws_muku_gal.csv',
                                         ps_start_date = ${s_start_date},
                                         ps_end_date = ${s_end_date},
                                         ps_input_file_progeny_flp_statement = '${s_input_file_progeny_flp_statement_ewbc}',
                                         ps_input_file_flp = 'zws_muku_flp.csv',
                                         ps_input_file_calf = '${s_input_file_calf}',
                                         ps_input_file_flp_carcass_matrix_statement = '${s_input_file_flp_carcass_matrix_statement_ewbc}',
                                         ps_input_file_price_cow = '${s_input_file_price_cow_ewbc}',
                                         ps_input_file_price_bull = '${s_input_file_price_bull_ewbc}',
                                         ps_input_file_price_heifer = '${s_input_file_price_heifer_ewbc}',
                                         ps_input_file_price_calf = '${s_input_file_price_calf}',
                                         ps_input_file_ped = 'ped_withBreedCode_damside',
                                         pb_log = ${b_log},
                                         plogger = NULL)"

      cd ${s_sirebreed}_${s_dambreed}_${s_prodsystem}_Natura-Beef
      # start ECOWEIGHT for beef-on-beef
      echo "${actual_date}_results.txt" | ewbc
      cd ${work_dir}

     fi

     if [ "$s_prodsystem" == 3 ]; then
        s_dambreed=KR

        echo "scenario: ${s_sirebreed}_${s_dambreed}_${s_prodsystem}_Natura-Beef " >> $Lauf_Bez.runLog
        # prepare ECOWEIGHT input-files for each scenario
        Rscript -e "qp4ewc::pre_process_ew_input(ps_sirebreed = '${s_sirebreed}',
                                 ps_dambreed = '${s_dambreed}',
                                 ps_prodsystew = '${s_prodsystem}',
                                 ps_marketchannel = 'Natura-Beef',
                                 ps_path_directory2create = '${s_path_directory2create}',
                                 ps_input_file_literature = '${s_input_file_literature_ewbc}',
                                 ps_input_file_par = '${s_input_file_par_ewbc}',
                                 ps_input_file_35 = '${s_input_file_35}',
                                 ps_input_file_36 = '${s_input_file_36}',
                                 ps_input_file_testedbulls = '${s_input_file_testedbulls}',
                                 ps_input_file_purchasedreplacementheifers = '${s_input_file_purchasedreplacementheifers}',
                                 ps_input_file_calving_statement = '${s_input_file_calving_statement_ewbc}',
                                 ps_input_file_calving = 'zws_muku_gal.csv',
                                 ps_start_date = ${s_start_date},
                                 ps_end_date = ${s_end_date},
                                 ps_input_file_progeny_flp_statement = '${s_input_file_progeny_flp_statement_ewbc}',
                                 ps_input_file_flp = 'zws_muku_flp.csv',
                                 ps_input_file_calf = '${s_input_file_calf}',
                                 ps_input_file_flp_carcass_matrix_statement = '${s_input_file_flp_carcass_matrix_statement_ewbc}',
                                 ps_input_file_price_cow = '${s_input_file_price_cow_ewbc}',
                                 ps_input_file_price_bull = '${s_input_file_price_bull_ewbc}',
                                 ps_input_file_price_heifer = '${s_input_file_price_heifer_ewbc}',
                                 ps_input_file_price_calf = '${s_input_file_price_calf}',
                                 ps_input_file_ped = 'ped_withBreedCode_damside',
                                 pb_log = ${b_log},
                                 plogger = NULL)"

      cd ${s_sirebreed}_${s_dambreed}_${s_prodsystem}_Natura-Beef
      # start ECOWEIGHT for beef-on-beef
      echo "${actual_date}_results.txt" | ewbc
      cd ${work_dir}

     fi
    done
done


echo "End qp4ewc::pre_process_ew_input() function for beef-on-beef (ewbc)" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog
echo "====================================================================================================" >> $Lauf_Bez.runLog


#' Pre-processing the input-parameter-file of ECOWEIGHT beef-on-dairy cattle (ewdc)
#+ pre_process_ewbc_input
echo "Start qp4ewc::pre_process_ew_input() function for beef-on-dairy (ewdc)" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog

for s_sirebreed in AN LM OB SI;do
    for s_dambreed in HO BS; do
        for s_marketingchannel in ConventionalVeal ConventionalBeef Export;do

        echo "scenario: ${s_sirebreed}_${s_dambreed}_4_${s_marketingchannel} " >> $Lauf_Bez.runLog
        # prepare ECOWEIGHT input-files for each scenario
        Rscript -e "qp4ewc::pre_process_ew_input(ps_sirebreed = '${s_sirebreed}',
                                         ps_dambreed = '${s_dambreed}',
                                         ps_prodsystew = '4',
                                         ps_marketchannel = '${s_marketingchannel}',
                                         ps_path_directory2create = '${s_path_directory2create}',
                                         ps_input_file_literature = '$par_dir/input_literature_${s_dambreed}_${s_marketingchannel}.txt',
                                         ps_input_file_par = '${s_input_file_par_ewdc}',
                                         ps_input_file_35 = '${s_input_file_35}',
                                         ps_input_file_36 = '${s_input_file_36}',
                                         ps_input_file_testedbulls = '${s_input_file_testedbulls}',
                                         ps_input_file_purchasedreplacementheifers = '${s_input_file_purchasedreplacementheifers}',
                                         ps_input_file_calving_statement = '${s_input_file_calving_statement_ewdc}',
                                         ps_input_file_calving = 'zws_muku_gal.csv',
                                         ps_start_date = ${s_start_date},
                                         ps_end_date = ${s_end_date},
                                         ps_input_file_progeny_flp_statement = '${s_input_file_progeny_flp_statement_ewdc}',
                                         ps_input_file_flp = 'zws_muku_flp.csv',
                                         ps_input_file_calf = '${s_input_file_calf}',
                                         ps_input_file_flp_carcass_matrix_statement = '${s_input_file_flp_carcass_matrix_statement_ewdc}',
                                         ps_input_file_price_cow = '${s_input_file_price_cow_ewdc}',
                                         ps_input_file_price_bull = '${s_input_file_price_bull_ewdc}',
                                         ps_input_file_price_heifer = '${s_input_file_price_heifer_ewdc}',
                                         ps_input_file_price_calf = '${s_input_file_price_calf}',
                                         ps_input_file_ped = 'ped_withBreedCode_damside',
                                         pb_log = ${b_log},
                                         plogger = NULL)"

        cd ${s_sirebreed}_${s_dambreed}_4_${s_marketingchannel}
        # start ECOWEIGHT for beef-on-dairy
        echo "${actual_date}_results.txt" | ewdc
        cd ${work_dir}

        done
    done
done


echo "End qp4ewc::pre_process_ew_input() function for beef-on-dairy (ewdc)" >> $Lauf_Bez.runLog
date >> $Lauf_Bez.runLog
echo "====================================================================================================" >> $Lauf_Bez.runLog

#********************************************************************************************************************************************
#' ## End of Script
#' The script ends here with an end message.
#+ end-msg, eval=FALSE
end_msg  >> $Lauf_Bez.runLog
