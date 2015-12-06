\name{reset_stream}
\alias{reset_stream}
\title{Reset a Data Stream to its Beginning}

\description{
Resets the counter in a DSD object to the beginning or any other position in
the stream.
}

\usage{
	reset_stream(dsd, pos = 1)
}

\arguments{
	\item{dsd}{An object of class a subclass of \code{DSD} which implements a 
  reset function.}
	\item{pos}{Position in the stream (the beginning of the stream is position 1).}
}

\details{
Resets the counter of the stream object.
For example, for\code{DSD_Memory}, the counter stored in the
environment variable is moved back to 1.
For \code{DSD_ReadCSV} objects, this is done by calling \code{seek()} on the
underlying connection. 
}

\seealso{
	\code{\link{DSD_ReadCSV}},
	\code{\link{DSD_MG}},
	\code{\link{DSD_ScaleStream}},
	\code{\link{DSD_Memory}}
}

\author{
    Michael Hahsler
}


\examples{
# initializing the objects
stream <- DSD_Gaussians(k=3, d=2)
replayer <- DSD_Memory(stream, 100)
replayer

p <- get_points(replayer, 50)
replayer

# reset replayer to the begining of the stream
reset_stream(replayer) 
replayer
  
# set replayer to position 21
reset_stream(replayer, pos=21)  
replayer
}
