% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcDT.R
\name{asLogicalDT}
\alias{asLogicalDT}
\title{Forcing the logical/boolean data type on a selected set of columns of a data.table object}
\usage{
asLogicalDT(inputDT, colNamesToBeTransformed = NULL)
}
\arguments{
\item{inputDT}{data.table object containing the data of interest. This is an obligatory argument, without default value.}

\item{colNamesToBeTransformed}{Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.}
}
\value{
No value is returned. Note that a valid value needs to be supplied to the 'colNamesToBeTransformed' argument in order to make this function work.
}
\description{
Forcing the logical/boolean data type on a selected set of columns of a data.table object
}
\examples{
library(data.table)
inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
asFactorDT(inputDT, c('y'))

\donttest{asLogicalDT(inputDT)}
asLogicalDT(inputDT, c('x', 'y'))

# First looking at the result, followed by testing if the transformation worked!

inputDT
isLogicalDT(inputDT, c('x', 'y'))

# Notice the 'funny' side effect for the 'F' character value of column y...
# This behavior is also observed for the as.logical() base R function.
}
