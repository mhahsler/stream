\docType{class}
\name{stream.SHC.clone-class}
\alias{stream.SHC.clone}
\alias{initialize,stream.SHC.clone-method}
\title{Statistical Hierarchical Clusterer - reference class, cloning construction}
\usage{
\S4method{initialize}{stream.SHC.clone}(old_shc)
}
\arguments{
\item{old_shc}{(SHC object) - An SHC object instantiated from the SHC reference classes (both \link{stream.SHC} and \link{stream.SHC.man}).}
}
\value{
Returns the cloned SHC object.
}
\description{
The Statistical Hierachical Clusterer reference class cloner.
}
\details{
Performs cloning of the existing SHC object, instantiated from the SHC reference class.
}
\examples{
s <- stream.SHC(2,driftType=SHCDriftType$NoDrift,decaySpeed=0,sigmaIndex=TRUE)
res <- s$process(data.frame(X=c(1,2,3,34,5,3,2,2,3,34,150),Y=c(3,4,2,1,6,7,4,5,6,3,150)))
s$getHistogram()
s_cloned <- stream.SHC.clone(s)
s_cloned$getHistogram()
}
\references{
[1] Krleža D, Vrdoljak B, and Brčić M, Statistical hierarchical clustering algorithm for outlier detection in evolving data streams, \emph{Machine Learning}, Sep. 2020
}
