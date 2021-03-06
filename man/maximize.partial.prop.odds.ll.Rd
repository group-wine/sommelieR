% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maximize.partial.prop.odds.ll.R
\name{maximize.partial.prop.odds.ll}
\alias{maximize.partial.prop.odds.ll}
\title{A function to maximize the partial proportional odds model log likelihood.}
\usage{
maximize.partial.prop.odds.ll(y, y.levels, in.data, int.vector,
  method = "BFGS", itnmax = NULL, x.prop.odds = NULL,
  x.non.prop.odds = NULL, beta.prop.odds = NULL,
  beta.non.prop.odds = NULL)
}
\arguments{
\item{y}{A vector of containing the values of an ordinal outcome variable.}

\item{y.levels}{A vector of the unique, ordinal levels of y.}

\item{in.data}{The input data object of type data frame or matrix.}

\item{int.vector}{A vector of intercept estimates (one for each level of the outcome variable, except the top level). These need to be in the order of the levels of the outcome variable (from low to high).}

\item{method}{A character specifying the optimization method to be used by package optimx in maximizing the log likelihood.}

\item{itnmax}{An optional scalar specifying the iteration limit used in maximizing the log likelihood. Defaults to the default optimx value for the given method.}

\item{x.prop.odds}{A design martrix (no intercept) for the variables assumed to have proportional odds.}

\item{x.non.prop.odds}{A design martrix (no intercept) for the variables assumed to not have proportional odds.}

\item{beta.prop.odds}{A vector of beta values for each predictor assumed to have proportional odds.}

\item{beta.non.prop.odds}{A matrix of beta values for each predictor assumed to not have proportional odds, where columns are the first j-1 levels of the ordinal outcome, and rows are betas.}
}
\value{
An object from package optimx containing the maximized log likelihood and parameter estimates.
}
\description{
This function attempts to maximize the partial proportional odds model log likelihood. This function is not intended to be run outside of function partial.prop.odds.mod.
}
