\name{evaluate}
\alias{evaluate}
\alias{evaluate_cluster}
\alias{print.stream_eval}
\title{Evaluate Clusterings}

\description{
	Gets evaluation measures for micro or macro-clusters from a DSC object
	given the original DSD object.
}

\usage{
evaluate(dsc, dsd, measure, n = 100, type=c("auto", "micro", "macro"), 
  assign="micro", assignmentMethod=c("auto", "model", "nn"), 
  noise = c("class", "exclude"), ...)
evaluate_cluster(dsc, dsd, measure, n = 1000,
  type=c("auto", "micro", "macro"), 
  assign="micro", assignmentMethod=c("auto", "model", "nn"),
  horizon=100, verbose=FALSE, noise = c("class", "exclude"), ...)
}

\arguments{
	\item{dsc}{The DSC object that the evaluation measure is being requested from.}
	\item{dsd}{The DSD object that holds the initial training data for the DSC.}
	\item{measure}{Evaluation measure(s) to use. If missing then all available
    measures are returned.}
	\item{n}{The number of data points being requested.}
	\item{type}{Use micro- or macro-clusters for evaluation. Auto
	        used the class of dsc to decide.}
	\item{assign}{Assign points to micro or macro-clusters?} 
	\item{assignmentMethod}{ How are points assigned to clusters for 
    evaluation (see \code{get_assignment})?}
	\item{horizon}{Evaluation is done using horizon many previous points
    (see detail section).}
	\item{verbose}{report progress?}
	\item{noise}{how to handle noise points in the data. Options are
	  to  treat as a separate class (default) or 
	  to exclude them from evaluation. }
	\item{...}{Unused arguments are ignored.}
}

\details{
	For evaluation each data points are assigned
	to its nearest cluster using Euclidean distance to the cluster
	centers. Then for each cluster the majority class is determined. Based on 
  the majority class several 
	evaluation measures can be computed.
  
  For \code{evaluate_cluster} the most commonly used method
  of prequential error estimation (see Gama, Sebastiao and Rodrigues; 2013). 
  The data points in the horizon are first used to calculate the 
  evaluation measire and then they are used for updating the cluster model.
  Many evaluation measures are calculated with code from the
  packages \pkg{cluster}, \pkg{clue} and \pkg{fpc}. Detailed documentation can 
  be found in these packages (see Section See Also.)
  
  The following information items are available:  
	\itemize{
		\item	\code{"numMicroClusters"} number of micro-clusters
    \item \code{"numMacroClusters"} number of macro-clusters 
		\item	\code{"numClasses"} number of classes
	}
  
  The following noise-related items are available:  
	\itemize{
	  \item	\code{"noisePredicted"} Number data points predicted as noise  
	  \item	\code{"noiseActual"} Number of data points which are actually noise
	  \item	\code{"noisePrecision"} Precision of the predicting noise
	  (i.e., number of correctly predicted noise points over the total number of 
	  points predicted as noise)
	}
  
  
  The following internal evaluation measures are available:
	\itemize{
		\item	\code{"SSQ"} within cluster sum of squares. Assigns each non-noise 
	  point to its nearest center from
	  the clustering and calculates the sum of squares 
		\item	\code{"silhouette"} average silhouette width (actual noise points   
		  which stay unassigned by the clustering algorithm are removed; regular 
		  points that are unassigned by the clustering algorithm 
		  will form their own noise cluster) (\pkg{cluster}) 
	  \item \code{"average.between"} average distance between clusters (\pkg{fpc})        
	  \item \code{"average.within"} average distance within clusters (\pkg{fpc})          
	  \item \code{"max.diameter"} maximum cluster diameter (\pkg{fpc})    
	  \item \code{ "min.separation"} minimum cluster separation (\pkg{fpc})
	  \item \code{"ave.within.cluster.ss"} a generalization of the within clusters sum of squares (half the sum of the within cluster squared dissimilarities divided by the cluster size) (\pkg{fpc})
	  \item \code{"g2"} Goodman and Kruskal's Gamma coefficient (\pkg{fpc}) 
    \item \code{"pearsongamma"} correlation between distances and a 0-1-vector where 0 means same cluster, 1 means different clusters (\pkg{fpc})
	  \item \code{"dunn"} Dunn index (minimum separation / maximum diameter) (\pkg{fpc}) 
    \item \code{"dunn2"} minimum average dissimilarity between two cluster / maximum average within cluster dissimilarity (\pkg{fpc}) 
	  \item \code{"entropy"} entropy of the distribution of cluster memberships (\pkg{fpc}) 
    \item \code{"wb.ratio"} average.within/average.between (\pkg{fpc})
	}  

  The following external evaluation measures are available:
  \itemize{
		\item	\code{"precision"}, \code{"recall"}, \code{"F1"} F1.
     A true positive (TP) decision assigns two points in the same
                true cluster also to the same cluster,
                a true negative (TN) decision assigns two points from two
                different true clusters to two different clusters.
                A false positive (FP) decision assigns two points from the
                same true cluster to two different clusters.
                A false negative (FN) decision assigns two points from the
                same true cluster to different clusters.

                precision = TP/(TP+FP)
                
                recall = TP/(TP+FN)

                The F1 measure is the harmonic mean of precision and recall.
                
		\item	\code{"purity"}  Average purity of clusters. The purity of each cluster
        is the proportion of the points of the majority true group assigned to it
        (see Cao et al. (2006))
%		\item	\code{"classPurity"} (of real clusters; see Wan et al (2009)), 
%		\item	\code{"fpr"} false positive rate,
		\item	\code{"Euclidean"} Euclidean dissimilarity of
		          the memberships (see Dimitriadou, Weingessel and Hornik
			            (2002)) (\pkg{clue}) 
		\item	\code{"Manhattan"} Manhattan dissimilarity of
		          the memberships (\pkg{clue}) 
		\item	\code{"Rand"} Rand index (see Rand (1971)) (\pkg{clue})
		\item	\code{"cRand"} Adjusted Rand index (see Hubert and Arabie (1985)) (\pkg{clue})
		\item	\code{"NMI"} Normalized Mutual Information (see Strehl and Ghosh
		          (2002)) (\pkg{clue})
		\item	\code{"KP"} Katz-Powell index (see Katz and
		          Powell (1953)) (\pkg{clue})
		\item	\code{"angle"} maximal cosine of the angle between the agreements (\pkg{clue})
		\item	\code{"diag"} maximal co-classification rate (\pkg{clue})
		\item	\code{"FM"} Fowlkes and Mallows's index (see Fowlkes and Mallows (1983)) (\pkg{clue})
		\item	\code{"Jaccard"} Jaccard index (\pkg{clue}) 
		\item	\code{"PS"} Prediction Strength (see Tibshirani and Walter (2005)) (\pkg{clue})
	%  \item  \code{"corrected.rand"} corrected Rand index (\pkg{fpc})
	  \item  \code{"vi"} 	variation of information (VI) index (\pkg{fpc})
}

Many measures are the average over all clusters. For example, purity is
the average purity over all clusters.

For \code{DSC_Micro} objects, data points are assigned to micro-clusters
and then each micro-cluster is evaluated. For \code{DSC_Macro} objects, 
data points by default (\code{assign="micro"}) also assigned to
micro-clusters, but these assignments are translated to macro-clusters.
The evaluation is here done for macro-clusters. This is important when 
macro-clustering is done with algorithms which do not create spherical
clusters (e.g, hierarchical clustering with single-linkage or DBSCAN) and
this assignment to the macro-clusters directly (i.e., their center) 
does not make sense.

Using \code{type} and \code{assign}, the user can select how to assign data
points and ad what level (micro or macro) to evaluate.

Many of the above measures are implemented package \pkg{clue} in function
\code{cl_agreement().}

\code{evaluate_cluster()} is used to evaluate an evolving data stream using
the method described by Wan et al. (2009). Of the \code{n} data points
\code{horizon} many points are clustered and then the evaluation
measure is calculated on the same data points. The idea is to find out if 
the clustering algorithm was able to adapt to the changing stream.
}

\value{
    \code{evaluate} returns an object of class \code{stream_eval}
    which is a numeric vector of the values of the requested measures
    and two attributes, \code{"type"} and
    \code{"assign"}, to see at what level the evaluation was done. 
}

\references{
Joao Gama, Raquel Sebastiao, Pedro Pereira Rodrigues (2013). On evaluating stream learning algorithms. Machine Learning, March 2013, Volume 90, Issue 3, pp 317-346.

F. Cao, M. Ester, W. Qian, A. Zhou (2006). Density-Based Clustering over an Evolving Data Stream with Noise. Proceeding of the 2006 SIAM Conference on Data Mining, 326-337.  
  
E. Dimitriadou, A. Weingessel and K. Hornik (2002).  A combination
scheme for fuzzy clustering.  International Journal of Pattern
Recognition and Artificial Intelligence, 16, 901-912.

E. B. Fowlkes and C. L. Mallows (1983).  A method for comparing
two hierarchical clusterings.  Journal of the American
Statistical Association, 78, 553-569.

L. Hubert and P. Arabie (1985).  Comparing partitions.  Journal
of Classification, 2, 193-218.

W. M. Rand (1971).  Objective criteria for the evaluation of
clustering methods.  Journal of the American Statistical
Association, 66, 846-850.

L. Katz and J. H. Powell (1953).  A proposed index of the
conformity of one sociometric measurement to another.
Psychometrika, 18, 249-256.

A. Strehl and J. Ghosh (2002).  Cluster ensembles - A knowledge
reuse framework for combining multiple partitions.  Journal of
Machine Learning Research, 3, 583-617.

R. Tibshirani and G. Walter (2005).  Cluster validation by
Prediction Strength.  Journal of Computational and Graphical
Statistics, 14/3, 511-528.

L Wan, W.K. Ng, X.H. Dang, P.S. Yu and K. Zhang (2009). Density-Based 
Clustering of Data Streams at Multiple Resolutions, 3(3).
}

\seealso{
            \code{\link{animate_cluster}},
            \code{\link[clue]{cl_agreement}} in \pkg{clue},
            \code{\link[fpc]{cluster.stats}} in \pkg{fpc},
            \code{\link[cluster]{silhouette}} in \pkg{cluster}.
}

\author{
    Michael Hahsler, Matthew Boanos and John Forrest
}


\examples{
stream <- DSD_Gaussians(k=3, d=2)

dstream <- DSC_DStream(gridsize=0.05, Cm=1.5)
update(dstream, stream, 500)
plot(dstream, stream)
# Evaluate micro-clusters 
# Note: we use here only n=500 points for evaluation to speed up execution
evaluate(dstream, stream, measure=c("numMicro","numMacro","purity","crand", "SSQ"), 
  n=100)

# DStream also provides macro clusters. Evaluate macro clusters with type="macro"
plot(dstream, stream, type="macro")
evaluate(dstream, stream, type ="macro", 
  measure=c("numMicro","numMacro","purity","crand", "SSQ"), n=100)

# Points are by default assigned to the closest micro clusters for evalution. 
# However, points can also be assigned to the closest macro-cluster using
# assign="macro".
evaluate(dstream, stream, type ="macro", assign="macro",
  measure=c("numMicro","numMacro","purity","crand", "SSQ"), n=100)

# Evaluate an evolving data stream
stream <- DSD_Benchmark(1)
dstream <- DSC_DStream(gridsize=0.05, lambda=0.1)
evaluate_cluster(dstream, stream, type="macro", assign="micro",
  measure=c("numMicro","numMacro","purity","crand"), 
  n=600, horizon=100)

\dontrun{
# animate the clustering process
reset_stream(stream)
dstream <- DSC_DStream(gridsize=0.05, lambda=0.1)
animate_cluster(dstream, stream, horizon=100, n=5000,  
  measure=c("crand"), type="macro", assign="micro",
  plot.args = list(type="both", xlim=c(0,1), ylim=c(0,1)))
}
}
