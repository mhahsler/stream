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
# this example requires DSC_MCOD in the streamMOA package
if(requireNamespace("streamMOA",quietly=TRUE)) {
  MySuperCallback <- function() {
    env <- environment()
    all_measures <- c("NoOfOutliers")
    internal_measures <- c()
    external_measures <- c(all_measures)
    outlier_measures <- c()
    this <- list(description = "My super callback", env = environment())
    class(this) <- c("MySuperCallback", "EvalCallback")
    this
  }
  evaluate_callback.MySuperCallback <- function(cb_obj, dsc, measure, points, actual, predict,
                                                outliers, predict_outliers,
                                                predict_outliers_corrid, centers, noise) {
    r <- list()
    if("NoOfOutliers" \%in\% measure) r$NoOfOutliers <- noutliers(dsc)
    r
  }
  stream <- DSD_Gaussians(k=3, d=5, o=1, outlier_horizon=100, separation=0.3)
  c <- streamMOA::DSC_MCOD(r=0.25, w=100, t=5)
  my_cb_obj <- MySuperCallback()
  evaluate_with_callbacks(c,stream,n=100,measure=c("cRand","NoOfOutliers"),
      callbacks=list(my_callback=my_cb_obj))
}
}
