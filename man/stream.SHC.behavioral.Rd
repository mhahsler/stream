\docType{class}
\name{stream.SHC-class}
\alias{stream.SHC}
\alias{initialize,stream.SHC-method}
\title{Statistical Hierarchical Clusterer - reference class, behavioral construction}
\usage{
\S4method{initialize}{stream.SHC}(dimensions,
  aggloType=SHCAgglomerationType$NormalAgglomeration,driftType=SHCDriftType$NormalDrift,
  decaySpeed=10,sharedAgglomerationThreshold=1,recStats=FALSE,sigmaIndex=FALSE,
  sigmaIndexNeighborhood=3,sigmaIndexPrecisionSwitch=TRUE)
}
\arguments{
\item{dimensions}{(integer) - A number of space dimensions.}
\item{aggloType}{(list, SHCAgglomerationType) - Agglomeration type: \emph{NormalAgglomeration,AggresiveAgglomeration,RelaxedAgglomeration}.}
\item{decaySpeed}{(integer) - Components decay speed. 0 = no decay, >0 = higher number represents slower decay.}
\item{driftType}{(list, SHCDriftType) - Drift type: \emph{NormalDrift,FastDrift,SlowDrift,NoDrift,UltraFastDrift}.}
\item{sharedAgglomerationThreshold}{(integer) - A number of data instances between components that cause their agglomeration under the same cluster.}
\item{recStats}{(logical) - A flag that indicated whether the SH clusterer should return statistics about the number of components and outliers generated during values processing.}
\item{sigmaIndex}{(logical) - A flag that indicates whether the SH clusterer should utilize the Sigma-index for speeing up the statistical space query.}
\item{sigmaIndexNeighborhood}{(integer) - A multiplier for the statistical neighborhood used to manage Sigma-index. This multiplier is used in conjunction with SH clusterer statistical thershold theta - hereby determined by the \emph{aggloType} parameter.}
\item{sigmaIndexPrecisionSwitch}{(logical) - A flag that indicates whether the Sigma-index should maintain high precision over a speed.}
}
\section{Methods}{
\describe{
\item{process(data)}{
  Initiates clustering for a suppled data frame (dataset). Returned data frame comprises macro and micro-level details, as well as outlier flags.
}
\item{getComponentAndOutlierStatistics()}{
  Returns the number of current components and outliers.
}
\item{getTimes()}{
  Returns the list of processing times during last clustering.
}
\item{getNodeCounter()}{
  Returns the number of statistical distance calculations when querying for statistical classification. Meaningful when utilizing sigma-index, to compare statistical neighborhood density for the processed dataset.
}
\item{getComputationCostReduction()}{
Can be used only with sigma-index. Returns the ratio of the number of statistical distance calculations and the maximal number of statistical distance calculations for the \emph{sequential scan} approach. The returned number tells how much statistical calculations was saved by utilizing simga-index for the processed dataset.
}
\item{getHistogram()}{
Returns computation cost reduction histogram when utilizing sigma-index.
}
\item{recheckOutlier(id, ...)}{
Can be used to re-check the outlier status for the supplied id. If the supplied id still represents an outlier, this method will return \emph{true}.
}
\item{getTrace(id)}{
Returns a list of current component identifiers that have trace back to the supplied component id. This method is used when we want to know new components to which are successors of the predecessor component that once had the supplied identifier. This method establishes direct temporal connection between the supplied component id and that returned list of current components.
}
\item{clearEigenMPSupport()}{
  Clears the OpenMP usage by the Eigen linear algebra package. Introduced only for the reproducibility purposes.
}
}}
\description{
The Statistical Hierachical Clusterer reference class.
}
\details{
Instantiates an SHC object that represents an instance of the stream.SHC reference class.
}
\examples{
s <- stream.SHC(2,driftType=SHCDriftType$NoDrift,decaySpeed=0,sigmaIndex=TRUE)
res <- s$process(data.frame(X=c(1,2,3,34,5,3,2,2,3,34,150),Y=c(3,4,2,1,6,7,4,5,6,3,150)))
res
s$getComponentAndOutlierStatistics()
s$recheckOutlier(res[11,"component_id"])
orig_id <- res[3,"component_id"]
trace_id <- s$getTrace(orig_id)
message(paste("Original id",orig_id,"traced to",trace_id))
s$getHistogram()
}
\references{
[1] Krleža D, Vrdoljak B, and Brčić M, Statistical hierarchical clustering algorithm for outlier detection in evolving data streams, \emph{Machine Learning}, Sep. 2020
}
