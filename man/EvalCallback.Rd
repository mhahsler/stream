\docType{class}
\name{EvalCallback-class}
\alias{EvalCallback}
\alias{evaluate_callback}
\title{Abstract Class for Evaluation Callbacks}
\section{Fields}{
\describe{
\item{all_measures}{
A list of all measures this object contributes to the evaluation. Union of all callback measures defines measures the end-user can use.
}
\item{internal_measures}{
A list of internal measures. A subset of \code{all_measures}.
}
\item{external_measures}{
A list of external measures. A subset of \code{all_measures}.
}
\item{outlier_measures}{
A list of outlier measures. A subset of \code{all_measures}.
}
}}
\section{Methods}{
\describe{
\item{\code{evaluate_callback(cb_obj, dsc, measure, points, actual, predict, outliers,
  predict_outliers, predict_outliers_corrid, centers, noise, ...)}}{
A method that allows callback for external clustering results evaluation.
  \itemize{
    \item{cb_obj - The callback object (EvalCallback).}
    \item{dsc - The clusterer object (DSC).}
    \item{measure - The requested measures.}
    \item{points - A data frame contining all data items.}
    \item{actual - Actual assignments for the related data instance in \code{points}, given by the used data stream generator.}
    \item{predict - Assignments for the related data instance in \code{points}, given by the clusterer.}
    \item{outliers - Outlier marks for the related data instance in \code{points}, marked by the used data stream generator.}
    \item{predict_outliers - Outlier marks for the related data instance in \code{points}, marked by the clusterer.}
    \item{predict_outliers_corrid - Outlier identifiers assigned by the clusterer.}
    \item{centers - Cluster centers given by the clusterer.}
    \item{noise - Noise assignments (NA) for the related data instance in \code{points}, for all data instances that cannot be classified neither into clusters or outliers.}
  }
}
}}
\description{
The abstract class for all evaluation callbacks. Cannot be instantiated. Must be inherited.
Evaluation is the process of the clustering quality assessment. This assessment can include clustering results, as well as the clustering process, e.g., duration, spatial query performance, and similar. The \emph{stream} package has some measurements (see \link{evaluate} for details) already implemented. All other measurements can be externally implemented without need to extend the \emph{stream} package, by using callbacks.
}
\author{
Dalibor Krleža
}
\examples{
CustomCallback <- function() {
  env <- environment()
  all_measures <- c("LowestWeightPercentage")
  internal_measures <- c()
  external_measures <- all_measures
  outlier_measures <- c()
  this <- list(description = "Custom evaluation callback",
               env = environment())
  class(this) <- c("CustomCallback", "EvalCallback")
  this
}
evaluate_callback.CustomCallback <- function(cb_obj, dsc, measure, points,
                                             actual, predict, outliers,
                                             predict_outliers,
                                             predict_outliers_corrid,
                                             centers, noise) {
    r <- list()
    if("LowestWeightPercentage" \%in\% measure)
        r$LowestWeightPercentage=min(get_weights(dsc))/sum(get_weights(dsc))
    r
}
stream <- DSD_Gaussians(k = 3, d = 2, p = c(0.2, 0.4, 0.4))
km <- DSC_Kmeans(3)
update(km, stream, n=500)
evaluate_with_callbacks(km, stream, type="macro", n=500,
                        measure = c("crand","LowestWeightPercentage"),
                        callbacks = list(cc=CustomCallback()))
}
