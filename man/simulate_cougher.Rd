% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_cougher.R
\name{simulate_cougher}
\alias{simulate_cougher}
\title{Generate a simulated cough timeline for a user}
\usage{
simulate_cougher(
  rate_mean,
  rate_variance = NULL,
  hours = 24 * 30,
  start_timestamp = as.numeric(Sys.time()),
  tz = Sys.timezone(),
  distribution = "nb",
  format_time = TRUE,
  random_seed = NULL
)
}
\arguments{
\item{rate_mean}{Mean hourly cough rate}

\item{rate_variance}{Variance in the hourly cough rate.
If left \code{NULL}, variance will be estimated using one of Hyfe's standard regression models,
which should produce realistic results for any scenario.}

\item{hours}{Duration of time series, in hours. Default is 720 (about a month).}

\item{start_timestamp}{Optional start timestamp (seconds since midnight UTC
on January 1, 1970). Default is to use your system's current time.}

\item{tz}{Optional timezone for timestamp. Default is your system timezone.}

\item{distribution}{The frequency distribution to base the simulation upon.
Default is \code{'nb'}, negative binomial. This is currently the only option.}

\item{format_time}{If \code{TRUE} (the default), formatted date/time variables will be returned using \code{format_hyfe_time()}.
It may be useful to change this to \code{FALSE} if conducting iterative analyses that rely on speed.}

\item{random_seed}{Set the random number generator seed in order to replicate a cougher.}
}
\value{
A dataframe with a \code{timestamp} column, a \code{coughs} column, and a dummy \code{rec} column in which all rows have 1 full hour of "recording".
If \code{format_time} is \code{TRUE}, additional date/time variables are provided. See \code{format_hyfe_time()}
or (this section of the vignette)\link{https://hyfe-ai.github.io/hyfer/#hyfetime}.
}
\description{
Simulate an hourly cough timeseries for an imaginary user.
}
