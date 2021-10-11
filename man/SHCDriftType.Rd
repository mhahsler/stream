\docType{class}
\name{SHCDriftType}
\alias{SHCDriftType}
\title{List of SHC Drift Templates}
\description{
Described in [1]. The following templates are available:
  \itemize{
    \item \strong{FastDrift} - \code{cbNLimit = 40}, \code{cbVarianceLimit = 6}, \code{driftCheckingSizeRatio = 1.0}, \code{driftMovementMDThetaRatio = 0.6}
    \item \strong{SlowDrift} - \code{cbNLimit = 120}, \code{cbVarianceLimit = 9}, \code{driftCheckingSizeRatio = 2.0}, \code{driftMovementMDThetaRatio = 1.0}
    \item \strong{NoDrift} - \code{cbNLimit = 0}, \code{cbVarianceLimit = 0}
    \item \strong{UltraFastDrift} - \code{cbNLimit = 20}, \code{cbVarianceLimit = 6}, \code{driftCheckingSizeRatio = 0.5}, \code{driftMovementMDThetaRatio = 0.5}
    \item \strong{NormalDrift} - \code{cbNLimit = 80}, \code{cbVarianceLimit = 8}, \code{driftCheckingSizeRatio = 1.3}, \code{driftMovementMDThetaRatio = 0.8}
  }
}
\author{
Dalibor Krleža
}
\references{
[1] Krleža D, Vrdoljak B, and Brčić M, Statistical hierarchical clustering algorithm for outlier detection in evolving data streams, \emph{Machine Learning}, Sep. 2020
}
