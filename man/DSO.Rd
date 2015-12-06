\name{DSO}
\alias{DSO}
\title{Data Stream Operator Base Classes}
\description{Abstract base classes for all DSO (Data Stream
Operator) classes.}

\details{
The \code{DSO} class cannot be instantiated 
(calling \code{DSO()} produces errors), 
but it serve as a base class from which other DSO classes 
inherit. 

Data stream operators use \code{update()} to process new data from 
the stream. The result of the operator can be optained via
\code{get_points()} and \code{get_weights()}.
}

\seealso{
	\code{\link[stream]{update}},
	\code{\link{get_points}},
	\code{\link{get_weights}},
	\code{\link{DSO_Window}},
	\code{\link{DSO_Sample}}
}

\author{
    Michael Hahsler
}

