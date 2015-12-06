\name{plot}
\alias{plot}
\alias{plot.DSD}
\alias{plot.DSC}
\title{Plotting Data Stream Data and Clusterings}
\description{Methods to plot data stream data and clusterings.}

\usage{
    \method{plot}{DSD}(x, n = 500, col= NULL, 
	    pch= NULL, ..., method="pairs", dim = NULL, alpha = 0.6)
    \method{plot}{DSC}(x, dsd = NULL, n = 500,
	    col_points=NULL, col_clusters=c("red", "blue"),
	    weights=TRUE, scale=c(1,5), cex=1, pch=NULL, method="pairs", 
	    dim = NULL, type = c("auto", "micro", "macro", "both"), 
      assignment=FALSE, ...)
}

\arguments{
	\item{x}{the DSD or DSC object to be plotted.}
	\item{dsd}{a DSD object to plot the data in the 
	    background.}
	\item{n}{number of plots taken from the dsd to plot.}
	\item{col, col_points, col_clusters}{colors
	    used for plotting.}
	\item{weights}{the size of the symbols for 
	    micro- and macro-clusters represents its weight.}
	\item{scale}{range for the symbol sizes used.}
	\item{cex}{size factor for symbols.}
	\item{pch}{symbol type.}
	\item{method}{method used for plotting: 
	    "pairs" (pairs plot), 
	    "scatter" (scatter plot) or 
	    "pc" (plot first 2 principal components).}
	\item{dim}{an integer vector with the dimensions to plot. If \code{NULL} 
      then for methods \code{"pairs"} and \code{"pc"} all dimensions are used 
      and for \code{"scatter"} the first two dimensions are plotted.}
	\item{alpha}{alpha shading used to plot the points.}
	\item{type}{Plot micro- or macro-clusters. Auto
      used the class of dsc to decide.}
	\item{assignment}{logical; show assignment area of micro-clusters. }
	\item{\dots}{further arguments are passed on to
      plot or pairs in \pkg{graphics}.}
}

%\details{
%}

%\value{
%}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSD}}
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k=3, d=3)

## plot data
plot(stream, n=500)
plot(stream, method="pc", n=500)
plot(stream, method="scatter", dim=c(1,3), n=500)

## create and plot micro-clusters
dstream <- DSC_DStream(gridsize=0.1)
update(dstream, stream, 500)
plot(dstream)

## plot with data, projected on the first two principal components 
## and dimensions 2 and 3
plot(dstream, stream)
plot(dstream, stream, method="pc")
plot(dstream, stream, dim=c(2,3))

## plot micro and macro-clusters 
plot(dstream, stream, type="both")
}
