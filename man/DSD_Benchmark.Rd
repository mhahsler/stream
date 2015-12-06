\name{DSD_Benchmark}
\alias{DSD_Benchmark}
\title{Data Stream Generator for Benchmark Data}
\description{
A data stream generator that generates 
several dynamic streams indented to be benchmarks to compare 
data stream clustering algorithms.
}

\usage{
DSD_Benchmark(i=1)
}
\arguments{
    \item{i}{number of the benchmark.}
}

\details{
Currently available benchmarks are 1 and 2.
}

\value{
Returns a \code{DSD} object.
}
	   
\seealso{
	\code{\link{DSD}}
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Benchmark(i=1)
\dontrun{
animate_data(stream, n=10000, horizon=100, xlim=c(0,1), ylim=c(0,1)) 
}
}
