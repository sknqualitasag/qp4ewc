% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input.R
\name{age_in_days}
\alias{age_in_days}
\title{Compute Age in Days}
\usage{
age_in_days(pdate_birth, pdate_today = lubridate::today(), pb_floor = TRUE)
}
\arguments{
\item{pdate_birth}{date of birth}

\item{pdate_today}{todays date}

\item{pb_floor}{should age in days be rounded down}
}
\value{
age in days
}
\description{
By default the age in days is computed. If age on different date should be
computed use pdate_today with a different values. In case you want to get
a real number as the age, then use pb_floor = FALSE.

The function is based on https://stackoverflow.com/questions/14454476/get-the-difference-between-dates-in-terms-of-weeks-months-quarters-and-years
}
