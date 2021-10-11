\docType{class}
\name{DSC_SHC.man-class}
\alias{DSC_SHC.man}
\alias{clearEigenMPSupport,DSC_SHC.man-method}
\title{Statistical Hierarchical Clusterer - manual construction}
\usage{
DSC_SHC.man(dimensions,theta,virtualVariance,parallelize=FALSE,
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
d <- DSD_Gaussians(k=2,d=2,outliers=2,separation_type="Mahalanobis",
    separation=4,space_limit=c(0,20),variance_limit=2,
    outlier_options=list(outlier_virtual_variance=2,outlier_horizon=10000))
c <- DSC_SHC.man(2,theta=3.2,virtualVariance=0.2,cbVarianceLimit=0,cbNLimit=0,decaySpeed=0)
update(c,d,n=10000)
reset_stream(d)
plot(c,d,n=10000)
}
\references{
[1] D. Krleža, B. Vrdoljak, and M. Brčić, Statistical hierarchical clustering algorithm for outlier detection in evolving data streams, \emph{Machine Learning}, Sep. 2020
}