\name{DSC_Micro}
\alias{DSC_Micro}
\title{Abstract Class for Micro Clusterers}
\description{Abstract class for all DSC Micro Clusterers.}

\details{
\code{DSC_Micro} cannot be instantiated. Calling \code{DSC_Micro()}
results in an error.
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler
}

\examples{
# available micro clusterers
DSC_registry$get_entries(DSC_Micro = TRUE)
}

