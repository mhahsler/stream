\docType{class}
\name{stream.SHC.man-class}
\alias{stream.SHC.man}
\alias{initialize,stream.SHC.man-method}
\title{Statistical Hierarchical Clusterer - reference class, manual construction}
\usage{
\S4method{initialize}{stream.SHC.man}(dimensions,theta,virtualVariance,parallelize=FALSE,
performSharedAgglomeration=TRUE,compAssimilationCheckCounter=50,
cbVarianceLimit=as.double(10.0),cbNLimit=as.integer(40),
driftRemoveCompSizeRatio=as.double(0.3),driftCheckingSizeRatio=as.double(1.3),
driftMovementMDThetaRatio=as.double(0.8),decaySpeed=as.integer(10),
sharedAgglomerationThreshold=as.integer(1),compFormingMinVVRatio=as.double(0.2),
compBlockingLimitVVRatio=as.double(0.0),recStats=FALSE,sigmaIndex=FALSE,
sigmaIndexNeighborhood=3,sigmaIndexPrecisionSwitch=TRUE)
}
\arguments{
\item{dimensions}{(integer) - A number of space dimensions.}
\item{theta}{(real) - A statistical distance threshold for component bounds.}
\item{virtualVariance}{(real) - A variance used to construct virtual covariance matrix surrounding outliers.}
\item{parallelize}{(logical) - Not implemented yet. Leave at FALSE.}
\item{performSharedAgglomeration}{(logical) - Depreciated. Use \emph{sharedAgglomerationThreshold} instead.}
\item{compAssimilationCheckCounter}{(integer) - A number of data instances between two component assimilation checks. A smaller component can be assimilated by a bigger component when the centroid of the smaller component get within bounds of the bigger components. With this parameter we define periodicity of such check. The smaller number, the slower SHC response.}
\item{cbVarianceLimit}{(real) - Minimal variance for component baseline snapshot. This parameter is closely related to the component baseline, which is related to the SHC drifting concept.}
\item{cbNLimit}{(integer) - Minimal population for component baseline snapshot. This parameter is closely related to the component baseline, which is related to the SHC drifting concept. Having \emph{cbVarianceLimit=0} and \emph{cbNLimit=0} means that driting mechanism is switched off.}
\item{driftRemoveCompSizeRatio}{(real) - Used in the SHC drifting mechanism [1]. After drifting was detected, new components that represent drifting front smaller than driftRemoveCompSizeRatio*population(original component) are removed. This parameter allows some cleaning of the space when having ultra-fast drifting populations.}
\item{driftCheckingSizeRatio}{(real) - Used in the SHC drifting mechanism [1]. When the biggest drifting child components reach driftCheckingSizeRatio*population(component baseline), we start calculating drifting index, to assess whether drift and split occured or not.}
\item{driftMovementMDThetaRatio}{(real) - Used in the SHC drifting mechanism [1]. Drifting index is calculated from the sub-clustered drifting components, i.e., dritfing front. We calculate mean weighted distance between sub-clustered components and the baseline components. If drifting index is bigger than driftMovementMDThetaRatio*theta, we consider that the drifting front is advancing away from the original component baseline position. Or that drifting front is splitting into several sub-populations.}
\item{decaySpeed}{(integer) - Components decay speed. 0 = no decay, >0 = higher number represents slower decay.}
\item{sharedAgglomerationThreshold}{(integer) - A number of data instances between components that cause their agglomeration under the same cluster.}
\item{compFormingMinVVRatio}{(real) - A minimal variance that needs to be attained in order SHC can promote an outlier into a newly formed components. This is used to prevent multiple observations having same values from forming a small component.}
\item{compBlockingLimitVVRatio}{(real) - A maximal variance for components. If a component reaches such variance it gets blocked, and all surrouding observations start forming new components.}
\item{recStats}{(logical) - A flag that indicated whether the SH clusterer should return statistics about the number of components and outliers generated during values processing.}
\item{sigmaIndex}{(logical) - A flag that indicates whether the SH clusterer should utilize the Sigma-index for speeing up the statistical space query.}
\item{sigmaIndexNeighborhood}{(integer) - A multiplier for the statistical neighborhood used to manage Sigma-index. This multiplier is used in conjunction with SH clusterer statistical thershold theta - hereby determined by the \emph{theta} parameter.}
\item{sigmaIndexPrecisionSwitch}{(logical) - A flag that indicates whether the Sigma-index should maintain precision over the speed.}
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
Instantiates an SHC object that represents an instance of the stream.SHC.man reference class.
}
\examples{
s <- stream.SHC.man(2,3.2,1.0,decaySpeed=0,cbNLimit=0,cbVarianceLimit=0,sigmaIndex=TRUE)
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
