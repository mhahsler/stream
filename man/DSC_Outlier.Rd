\docType{class}
\name{DSC_Outlier-class}
\alias{DSC_Outlier}
\alias{get_outlier_positions}
\alias{noutliers}
\alias{recheck_outlier}
\title{Abstract Class for Outlier Detection Clusterers}
\section{Methods}{
\describe{
\item{\code{clean_outliers(x, ...)}}{
A method that requires removal of all outliers from the addressed outlier detection clusterer.
}
\item{\code{get_outlier_positions(x, ...)}}{
Returns spatial positions of all current outliers.
}
\item{\code{recheck_outlier(x, outlier_correlated_id, ...)}}{
Re-checks the outlier having \code{outlier_correlated_id}. If this object is still an outlier, the method returns TRUE.
}
\item{\code{noutliers(x, ...)}}{
Returns the current number of outliers.
}
\item{\code{print(x, ...)}}{
Prints out the general info and statistics about the outlier detection clusterer.
}
\item{\code{get_assignment(x, points, type=c("auto", "micro", "macro"),
                           method=c("auto", "nn", "model"), outlier_threshold=0.05, ...)}}{
Calculates assignments and outlier marks for the \code{points} data frame. Points are assigned to an outlier only if they are inside \code{outlier_threshold} distance from its position. This implementation is heavily dependant on the Euclidean distance measure and should be overwritten by concrete outlier detection clusterer implementations.
}
}}
\description{
The abstract class for all outlier detection clusterers. Cannot be instantiated. Must be inherited.
}
\author{
Dalibor Krleža
}
