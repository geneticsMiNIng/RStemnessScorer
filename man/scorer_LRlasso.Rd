% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scorer_LRlasso.R
\name{scorer_LRlasso}
\alias{scorer_LRlasso}
\title{Lasso Scorer}
\usage{
scorer_LRlasso(xtest, object, resp = 2)
}
\arguments{
\item{xtest}{dataset to test}

\item{object}{model created by \code{\link{buildScorer}(model='LR')}.}

\item{resp}{which from object$classes or object$glmnet.fit$classnames
is the response.}

\item{alpha}{take these adjusted p-values that are smaller than alpha.}
}
\value{
Vector of scores for each observation. The score is a value
between 0 and 1.
}
\description{
Get the signture from Wilcoxon Multiple Comparisions Test.
}
\author{
Katarzyna Sobiczewska
}

