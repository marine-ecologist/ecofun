% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_repeat_grid.R
\name{generate_repeated_grid}
\alias{generate_repeated_grid}
\title{Wrapper to simplify INLA models}
\usage{
generate_repeated_grid(data, predictors, n = 1)
}
\arguments{
\item{data}{Data frame for INLA model}

\item{predictors}{Specified formula (no random effects yet)}

\item{n}{New data for predictors as a list}

\item{...}{Additional parameters passed to INLA}
}
\description{
Function to wrap INLA models with predictors
}
