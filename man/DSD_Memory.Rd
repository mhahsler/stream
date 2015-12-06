\name{DSD_Memory}
\alias{DSD_Memory}
\title{A Data Stream Interface for Data Stored in Memory}
\description{
This class provides a data stream interface for data stored in memory as 
matrix-like objects (including data frames).
All or a portion of the stored data can be replayed several times.
}

\usage{DSD_Memory(x, n, k=NA, loop=FALSE, 
  class = NULL, description=NULL)}

\arguments{
	\item{x}{A matrix-like object containing the data. 
	If \code{x} is a DSD object then a data frame for \code{n} data points
	from this DSD is created. }
	\item{n}{Number of points used if \code{x} is a DSD object. If
	\code{x} is a matrix-like object then \code{n} is ignored.}
	\item{k}{Optional: The known number of clusters in the data}
	\item{loop}{Should the stream start over when it reaches the end?}
	\item{class}{Vector with the class/cluster label (only used if \code{x} is not
	  a DSD object). }
	\item{description}{character string with a description. }
}

\details{
In addition to regular data.frames other matrix-like objects that provide subsetting with the bracket operator can be used.
This includes \code{ffdf} (large data.frames 
stored on disk) from package \pkg{ff} 
and \code{big.matrix} from \pkg{bigmemory}.
}

\value{
Returns a \code{DSD_Memory} object 
(subclass of \code{DSD_R}, \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
	\code{\link{reset_stream}}
}

\author{
    Michael Hahsler
}

\examples{
# store 1000 points from a stream
stream <- DSD_Gaussians(k=3, d=2)
replayer <- DSD_Memory(stream, k=3, n=1000)
replayer
plot(replayer)  
  
# creating 2 clusterers of different algorithms
dsc1 <- DSC_DBSTREAM(r=0.1)
dsc2 <- DSC_DStream(gridsize=0.1, Cm=1.5)
  
# clustering the same data in 2 DSC objects
reset_stream(replayer) # resetting the replayer to the first position
update(dsc1, replayer, 500)
reset_stream(replayer)
update(dsc2, replayer, 500)
  
# plot the resulting clusterings
reset_stream(replayer) 
plot(dsc1, replayer, main="DBSTREAM")
reset_stream(replayer) 
plot(dsc2, replayer, main="D-Stream")   
  
### use a data.frame to create a stream (3rd col. contains the assignment)
df <- data.frame(x=runif(100), y=runif(100), 
  class=sample(1:3, 100, replace=TRUE))
head(df)  

stream <- DSD_Memory(df[,c("x", "y")], class=df[,"class"])  
stream
}
