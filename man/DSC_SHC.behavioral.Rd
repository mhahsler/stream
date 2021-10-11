\docType{class}
\name{DSC_SHC.behavioral-class}
\alias{DSC_SHC.behavioral}
\alias{clearEigenMPSupport,DSC_SHC.behavioral-method}
\alias{clearEigenMPSupport}
\title{Statistical Hierarchical Clusterer - behavioral construction}
\usage{
DSC_SHC.behavioral(dimensions,aggloType=SHCAgglomerationType$NormalAgglomeration,
driftType=SHCDriftType$NormalDrift,decaySpeed=10,sharedAgglomerationThreshold=1,
recStats=FALSE,sigmaIndex=FALSE,sigmaIndexNeighborhood=3,sigmaIndexPrecisionSwitch=TRUE)
}
\arguments{
\item{dimensions}{(integer) - A number of space dimensions.}
\item{aggloType}{(list, SHCAgglomerationType) - Agglomeration type: \emph{NormalAgglomeration,AggresiveAgglomeration,RelaxedAgglomeration}.}
\item{driftType}{(list, SHCDriftType) - Drift type: \emph{NormalDrift,FastDrift,SlowDrift,NoDrift,UltraFastDrift}.}
\item{decaySpeed}{(integer) - Components decay speed. 0 = no decay, >0 = higher number represents slower decay.}
\item{sharedAgglomerationThreshold}{(integer) - A number of data instances between components that cause their agglomeration under the same cluster.}
\item{recStats}{(logical) - A flag that indicated whether the SH clusterer should return statistics about the number of components and outliers generated during values processing.}
\item{sigmaIndex}{(logical) - A flag that indicates whether the SH clusterer should utilize the Sigma-index for speeing up the statistical space query.}
\item{sigmaIndexNeighborhood}{(integer) - A multiplier for the statistical neighborhood used to manage Sigma-index. This multiplier is used in conjunction with SH clusterer statistical thershold theta - hereby determined by the \emph{aggloType} parameter.}
\item{sigmaIndexPrecisionSwitch}{(logical) - A flag that indicates whether the Sigma-index should maintain precision over the speed.}
}
\section{Methods}{
\describe{
All methods here are detailed in the stream package.
\item{\code{get_stats(dsc,...)}}{
  Returns components and outliers statistics for the last data processing. Only when \emph{recStats=T}.
}
\item{\code{\link[stream]{get_microclusters}(dsc,...)}}{
  Returns a list of current component centers.
}
\item{\code{\link[stream]{get_microweights}(dsc,...)}}{
  Returns the list of current component weights, based on the component population divided by the total population.
}
\item{\code{\link[stream]{get_macroclusters}(dsc,...)}}{
  Returns a list of current cluster centers.
}
\item{\code{\link[stream]{get_macroweights}(dsc,...)}}{
  Returns a list of current cluster weights, based on the cluster population (sum of component populations) divided by the total population..
}
\item{\code{\link[stream]{microToMacro}(dsc,micro=NULL,...)}}{
  Returns a mapping between components and clusters.
}
\item{\code{\link[stream]{get_assignment}(dsc, points, type=c("auto", "micro", "macro"), method=c("auto", "model", "nn"), ...)}}{
  Returns assignments of points to component and clusters, depending on the \emph{type} parameter value. Returns a data frame with assignment values, and possibly additional attributes that indicate outliers and their SHC internal identifiers.
}
\item{\code{get_outlier_positions(dsc, ...)}}{
  Returns a list of outliers and their positions.
}
\item{\code{recheck_outlier_positions(dsc, outlier_correlated_id, ...)}}{
  Performs re-checking of the previously encountered outlier. An SHC outlier identifier must be supplied. Function returns TRUE when the outlier still stands (or decayed), or FALSE if it was assimilated by some component in the meantime.
}
\item{\code{clearEigenMPSupport(dsc, ...)}}{
  Clears the OpenMP usage by the Eigen linear algebra package. Introduced only for the reproducibility purposes.
}}}
\description{
The Statistical Hierachical Clusterer class.
}
\details{
Instantiates an SHC \link[stream]{DSC} object that represents an extension of abstract classes in the stream package, namely \link[stream]{DSC_SinglePass}, \link[stream]{DSC_Outlier}, \link[stream]{DSC_Macro}, and \link[stream]{DSC_R}. This object can be used in all stream package methods.
}
\examples{
d <- DSD_Gaussians(k=2,d=2,outliers=2,separation_type="Mahalanobis",separation=4,
                   space_limit=c(0,15),outlier_options=list(outlier_horizon=10000))
c <- DSC_SHC.behavioral(2,driftType = SHCDriftType$NoDrift,decaySpeed = 0)
update(c,d,n=10000)
reset_stream(d)
plot(c,d,n=10000)
}
\references{
[1] Krleža D, Vrdoljak B, and Brčić M, Statistical hierarchical clustering algorithm for outlier detection in evolving data streams, \emph{Machine Learning}, Sep. 2020
}
