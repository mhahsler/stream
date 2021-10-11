\docType{class}
\name{SHCAgglomerationType}
\alias{SHCAgglomerationType}
\title{List of SHC Agglomeration Templates}
\description{
Described in [1]. The following templates are available:
  \itemize{
    \item \strong{NormalAgglomeration} - \code{theta = 3.2}, \code{virtualVariance = 1.0}
    \item \strong{AggresiveAgglomeration} - \code{theta = 3.5}, \code{virtualVariance = 1.2}
    \item \strong{RelaxedAgglomeration} - \code{theta = 2.9}, \code{virtualVariance = 0.8}
  }
}
\author{
Dalibor Krleža
}
\examples{
set.seed(0)
d <- DSD_Gaussians(k = 3, d = 2, outliers = 5, separation_type = "Mahalanobis",
    separation = 6, space_limit = c(0, 50), variance_limit = 2,
    outlier_options = list(outlier_horizon = 10000,outlier_virtual_variance = 2))
c <- DSC_SHC.behavioral(2, aggloType = SHCAgglomerationType$AggresiveAgglomeration, decaySpeed = 0,
      sigmaIndex = TRUE)
evaluate(c, d, n = 10000, type = "macro", measure = c("crand", "outlierjaccard"))

set.seed(0)
d <- DSD_Gaussians(k = 3, d = 2, outliers = 5, separation_type = "Mahalanobis",
    separation = 6, space_limit = c(0, 50), variance_limit = 2,
    outlier_options = list(outlier_horizon = 10000,outlier_virtual_variance = 2))
c <- DSC_SHC.behavioral(2, aggloType = SHCAgglomerationType$RelaxedAgglomeration, decaySpeed = 0,
      sigmaIndex = TRUE)
evaluate(c, d, n = 10000, type = "macro", measure = c("crand", "outlierjaccard"))
}
\references{
[1] Krleža D, Vrdoljak B, and Brčić M, Statistical hierarchical clustering algorithm for outlier detection in evolving data streams, \emph{Machine Learning}, Sep. 2020
}
