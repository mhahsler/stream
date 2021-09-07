\name{DSC}
\alias{DSC}
\alias{DSC_R}
\alias{get_microclusters}
\alias{get_microweights}
\alias{get_macroclusters}
\alias{get_macroweights}
\alias{DSC_registry}
\title{Data Stream Clusterer Base Classes}
\description{Abstract base classes for all DSC (Data Stream
Clusterer) and DSC_R classes. Concrete implementations can be found in the registry \code{DSC_registry} (See Examples Section).}

\details{
The \code{DSC} and \code{DSC_R} classes cannot be instantiated
(calling \code{DSC()} or \code{DSC_R()} produces errors),
but they serve as a base class from which other DSC classes
inherit. Implementations can be found using the DSC registry \code{DSC_registry} (See Examples section).

Class \code{DSC} provides several generic functions that can
operate on all DSC subclasses:
\code{print()}, \code{plot()}, \code{nclusters()} to
get the current number of clusters,
\code{get_centers()} to get the cluster centers,
and \code{get_weights()} to get the cluster weights
(if implemented). \code{get_centers()} and \code{get_weights()}
are typically overwritten by subclasses of \code{DSC}. \code{DSC_R}
provides these functions for R-based DSC implementations.

Since \code{DSC} objects often contain external pointers, regular saving
and reading operations will fail. Use \code{saveDSC()} and \code{readDSC()}
which will serialize the objects first appropriately.
}

\seealso{
	\code{\link{animate_cluster}},
	\code{\link{cluster}},
	\code{\link{evaluate}},
	\code{\link{get_assignment}},
	\code{\link{get_centers}},
	\code{\link{get_weights}},
	\code{\link{get_copy}},
	\code{\link{microToMacro}},
	\code{\link{nclusters}},
	\code{\link[stream]{plot}},
	\code{\link{prune_clusters}},
	\code{\link{recluster}},
	\code{\link{readDSC}},
	\code{\link{saveDSC}}
}

\author{
    Michael Hahsler
}
\examples{
DSC_registry

# all clusterers
DSC_registry$get_entries()

# available micro clusterers
DSC_registry$get_entries(DSC_Micro = TRUE)

# available macro clusterers for reclustering
DSC_registry$get_entries(DSC_Macro = TRUE)

# find DSC by name
DSC_registry$get_entries("DBSTREAM")
}
