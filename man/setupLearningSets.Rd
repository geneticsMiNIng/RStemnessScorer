% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setupLearningSets.R
\name{setupLearningSets}
\alias{setupLearningSets}
\title{Setup Train & Test}
\usage{
setupLearningSets(pcbc, tcga, signature, G = "SC", cutoff = 0.7)
}
\arguments{
\item{pcbc}{data set containing in the first column labels
given as factor. At least one level of the factor should be G. The
next columns are for predictors.}

\item{tcga}{tcga data set with healthy tissues.
The columns are predictors.}

\item{signature}{vector of characters with names of predictors that
might be used by the training data.}

\item{G}{label name for selected group of samples in \code{pcbc} given
as character.}
}
\value{
List of data sets as follows:
list(train = list(X=..., Y=...), test = list(X=..., Y=...))
}
\description{
Combining PCBC and TCGA data as one training data set.
}
\examples{
wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
sign <- signatureWMCT(wmct, n=10)
train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
m <- glmnet::glmnet(as.matrix(train$X), train$Y, family='binomial')
plot(m)
}
\author{
Katarzyna Sobiczewska
}

