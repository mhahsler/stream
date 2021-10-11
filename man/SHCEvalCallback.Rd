\docType{class}
\name{SHCEvalCallback-class}
\alias{SHCEvalCallback}
\title{Statistical Hierarchical Clusterer Evaluation Callbacks}
\section{Fields}{
\describe{
\item{all_measures}{
  The following list of measures are available:
  \itemize{
    \item \emph{queryTime} - Total query time for the last processing.
    \item \emph{updateTime} - Total sigma-index update time.
    \item \emph{processTime} - Total time needed for the last processing... measured inside SHC algorithm.
    \item \emph{nodeCount} - Total mahalanobis distances calculated for the last processing.
    \item \emph{computationCostReduction} - Computational cost reduction when using sigma-index comparing to the sequential scan.
  }
}
\item{internal_measures}{
Same as \code{all_measures}.
}
\item{external_measures}{
Empty.
}
\item{outlier_measures}{
Empty.
}
}}
\description{
Allows SHC evaluation callback for the \pkg{stream} package. This call contains measures mostly related to use of the sigma-index.
}
\author{
Dalibor Krleža
}
\examples{
set.seed(0)
d <- DSD_Gaussians(k = 3, d = 2, outliers = 5, separation_type = "Mahalanobis", 
      separation = 4, space_limit = c(0, 25), variance_limit = 1,
      outlier_options = list(outlier_horizon = 10000))
c <- DSC_SHC.man(2, 3.2, 0.3, cbNLimit = 0, cbVarianceLimit = 0,decaySpeed = 0,
      sigmaIndex = TRUE)
evaluate_with_callbacks(c, d, n = 10000, type = "macro", 
      measure = c("crand", "outlierjaccard", "computationCostReduction"), 
      callbacks = list(shc = SHCEvalCallback()))
}
