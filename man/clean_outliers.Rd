\name{clean_outliers}
\alias{clean_outliers}
\alias{clean_outliers.DSOutlier}
\title{Clean Outliers from the Outlier Detecting Clusterer}

\description{
Removes all outliers from the outlier detecting clusterer.
}

\usage{
clean_outliers(x, ...)
\method{clean_outliers}{DSOutlier}(x, ...)
}
\arguments{
	\item{x}{The DSC object.}
	\item{...}{Additional parameters to pass to \code{clean_outliers}
	implementations.}
}
\details{
Removes all outliers from the outlier detecting clusterer.
}

\seealso{
\code{\link{DSOutlier}}
}

\author{
Dalibor Krleža
}
