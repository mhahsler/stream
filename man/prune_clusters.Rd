\name{prune_clusters}
\alias{prune_clusters}
\title{Prune Clusters from a Clustering}

\description{
Creates a (static) copy of a clustering where 
a fraction of the weight or the number of 
clusters with the lowest weights were pruned.
}

\usage{
	prune_clusters(dsc, threshold= 0.05, weight = TRUE)
}

\arguments{
	\item{dsc}{The DSC object to be pruned.}
	\item{threshold}{The numeric vector of probabilities for the quantile.}
	\item{weight}{should a fraction of the total weight in the clustering
	be pruned? Otherwise a fraction of clusters is pruned.}
}

%\details{
%}

\value{
Returns an object of class
\code{DSC_Static}.
}

\seealso{
	\code{\link{DSC_Static}}
}

\author{
    Michael Hahsler
}


\examples{
# 3 clusters with 10% noise
stream <- DSD_Gaussians(k=3, noise=0.1)

dbstream <- DSC_DBSTREAM(r=0.1)
update(dbstream, stream, 500)
dbstream
plot(dbstream, stream)

# prune lightest micro-clusters for 20% of the weight of the clustering
static <- prune_clusters(dbstream, threshold=0.2)
static
plot(static, stream)
}
