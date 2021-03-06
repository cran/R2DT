% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcDT.R
\name{isLogicalDT}
\alias{isLogicalDT}
\title{Testing if a set of columns of a data.table object corresponds to the logical/boolean data type}
\usage{
isLogicalDT(inputDT, colNamesToBeChecked = NULL, returnNames = FALSE)
}
\arguments{
\item{inputDT}{data.table object containing the data of interest. This is an obligatory argument, without default value.}

\item{colNamesToBeChecked}{Character vector containing potential column names of the 'inputDT' argument. The default value is NULL.}

\item{returnNames}{Logical vector of length 1 indicating whether or not the column name of the selected booleans should be returned. The default value is FALSE.}
}
\value{
A logical vector of length the size of the 'colNamesToBeChecked' argument, or in the absence of a value the number of columns of the 'inputDT' argument, that is TRUE if the corresponding column of the 'inputDT' argument is a boolean. If the 'returnNames' argument equals TRUE, then only those column names from the aforementioned selection of column of the 'inputDT' argument are returned that are a boolean.
}
\description{
Testing if a set of columns of a data.table object corresponds to the logical/boolean data type
}
\examples{
library(data.table)
inputDT <- as.data.table(data.frame(x = rep(c(TRUE, FALSE), 5), y = LETTERS[1:10]))
asFactorDT(inputDT, c('y'))

isLogicalDT(inputDT)
isLogicalDT(inputDT, c('x', 'y'))
isLogicalDT(inputDT, returnNames = TRUE)

isLogicalDT(inputDT, 'x')
\donttest{isLogicalDT(inputDT, c('x', 'y1'))}
}
