\name{recluster}
\alias{recluster}
\title{Re-clustering micro-clusters}

\description{Use a macro clustering algorithm to recluster micro-clusters into a final clustering.
}

\usage{
recluster(macro, micro, type="auto", ...)
}

\arguments{
  \item{macro}{a macro clustering algorithm (class "DSC_Macro")}
  \item{micro}{a DSC object containing micro-clusters. }
  \item{type}{controls which clustering is used from \code{dsc} 
    (typically micro-clusters).}
  \item{...}{additional arguments passed on.}
}

\details{
Takes centers and weights of the micro-clusters and applies the macro clustering algorithm.
}

\value{
The object macro is altered and contains the clustering.
}


%\seealso{
%}

\author{
    Michael Hahsler
}


\examples{
set.seed(0)
### create a data stream and a micro-clustering
stream <- DSD_Gaussians(k=3, d=3)

sample <- DSC_Sample(k=50)
update(sample, stream, 500)
sample
  
### recluster using k-means
kmeans <- DSC_Kmeans(k=3)
recluster(kmeans, sample)

### plot clustering
plot(kmeans, stream, main="Macro-clusters (Sampling + k-means)")
}
