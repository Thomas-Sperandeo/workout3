% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_variable}
\alias{bin_variable}
\title{bin_variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{a non-negative integer which provides the number of trials}

\item{prob}{a number between 0 and 1 which gives the probablity of a success on an individual draw}
}
\value{
an object of binvar class, number of trials and the probability of an individual success, an object of class \code{"binvar"}
}
\description{
function which checks if our variable is binomial and dispays parameters
}
\examples{

#default
prob1 <- bin_variable(1, 0.5)

#example 2
prob2 <- bin_variable(10, 0.3)

}
