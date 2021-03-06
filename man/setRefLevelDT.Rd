% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcDT.R
\name{setRefLevelDT}
\alias{setRefLevelDT}
\title{Setting the reference level of all or a selected set of the factor columns of a data.table object}
\usage{
setRefLevelDT(inputDT, categoricalVar, referenceLevel)
}
\arguments{
\item{inputDT}{data.table object containing the data of interest. This is an obligatory argument, without default value.}

\item{categoricalVar}{Character vector containing potential column names of the 'inputDT' argument. This is an obligatory argument, without default value.}

\item{referenceLevel}{Character vector containing the new reference levels. This is an obligatory argument, without default value.}
}
\value{
No value is returned. Note that the 'categoricalVar' and 'referenceLevel' should match up, meaning that they should be of the same length and the ith element should refer to the same variable.
}
\description{
Setting the reference level of all or a selected set of the factor columns of a data.table object
}
\examples{
library(data.table)
inputDT <- as.data.table(data.frame(x = LETTERS[11:20], y = LETTERS[1:10]))
asFactorDT(inputDT, c('x', 'y'))

\donttest{setRefLevelDT(inputDT)}

levels(inputDT$x)[1]
levels(inputDT$y)[1]
setRefLevelDT(inputDT, c('x', 'y'), c('L', 'C'))
levels(inputDT$x)[1]
levels(inputDT$y)[1]

\donttest{setRefLevelDT(inputDT, c('x', 'y'), c('bla', 'bla'))}

inputDT <- as.data.table(data.frame(x = seq(1, 20, 2), y = LETTERS[1:10]))
asFactorDT(inputDT, c('y'))
levels(inputDT$y)[1]
setRefLevelDT(inputDT, 'y', 'E')
levels(inputDT$y)[1]
}
