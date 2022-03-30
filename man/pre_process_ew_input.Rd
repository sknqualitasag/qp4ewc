% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_processing.R
\name{pre_process_ew_input}
\alias{pre_process_ew_input}
\title{Pre-processing the input-parameter-file of ECOWEIGHT}
\usage{
pre_process_ew_input(
  ps_sirebreed,
  ps_prodsystew,
  ps_marketchannel,
  ps_path_directory2create,
  ps_input_file_literature,
  pb_log = FALSE,
  plogger = NULL
)
}
\arguments{
\item{ps_sirebreed}{sire breed}

\item{ps_prodsystew}{production system build up as option in ECOWEIGHT}

\item{ps_marketchannel}{market channel}

\item{ps_path_directory2create}{path of the directory that will be created}

\item{ps_input_file_literature}{path to file with input coming from literature for the input-parameter-file for ECOWEIGHT}

\item{pb_log}{indicator whether logs should be produced}

\item{plogger}{logger object}
}
\description{
The program package ECOWEIGHT (C Programs for Calculating Economic Weights in Livestock)
need input parameter files. This function processed different functions
to prepare the input parameter files.
}