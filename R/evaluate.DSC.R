#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

## evaluate clusterings
## FIXME: calculate dist only once

#' Evaluate Stream Clustering
#'
#' Calculate evaluation measures for micro or macro-clusters from a [DSC] object given
#' the original [DSD] object.
#'
#' For evaluation, each data point is assigned to its nearest cluster using
#' Euclidean distance to the cluster centers. Then for each cluster the
#' majority class is determined. Based on the majority class several evaluation
#' measures can be computed.
#'
#' We provide two evaluation methods:
#'
#' - `evaluate_static()` evaluates the current static clustering using new data without updating the model.
#' - `evaluate_stream()` evaluates the clustering process using
#'   _prequential error estimation_ (see Gama, Sebastiao and Rodrigues; 2013).  The current model is
#'   first applied to the data points in the horizon to calculate the evaluation measures. Then, the
#'   cluster model is updated with the points.
#'
#' **Evaluation Measures**
#'
#' Many evaluation measures are available using
#' code from other packages including [cluster::silhouette()],
#' [clue:: cl_agreement()], and [fpc::cluster.stats()].
#'
#' The following information items are available:
#'
#'  - `"numPoints"` number of points used for evaluation.
#'  - `"numMicroClusters"` number of micro-clusters
#'  - `"numMacroClusters"` number of macro-clusters
#'  - `"numClasses"` number of classes
#'
#' The following noise-related/outlier items are available:
#'
#'  - `"noisePredicted"` Number data points predicted as noise
#'  - `"noiseActual"` Number of data points which are actually noise
#'  - `"noisePrecision"` Precision of the predicting noise (i.e., number of
#'     correctly predicted noise points over the total number of points predicted
#'     as noise)
#'  - `"outlierJaccard"` - A variant of the Jaccard index used to assess
#'     outlier detection accuracy (see Krleza et al (2020)).  Outlier Jaccard index
#'     is calculated as `TP / (TP + FP + UNDETECTED)`.
#'
#' The following internal evaluation measures are available:
#'  - `"SSQ"` within cluster sum of squares. Assigns each point to
#'     its nearest center from the clustering and calculates the sum of squares.
#'     Noise points in the data stream are always ignored.
#'  - `"silhouette"` average silhouette width. Actual noise points
#'     which stay unassigned by the clustering algorithm are ignored; regular
#'     points that are unassigned by the clustering algorithm form their own
#'     noise cluster) (\pkg{cluster})
#'  - `"average.between"` average distance between clusters (\pkg{fpc})
#'  - `"average.within"` average distance within clusters (\pkg{fpc})
#'  - `"max.diameter"` maximum cluster diameter (\pkg{fpc})
#'  - `"min.separation"` minimum cluster separation (\pkg{fpc})
#'  - `"ave.within.cluster.ss"` a generalization
#'      of the within clusters sum of squares (half the sum of the within cluster
#'      squared dissimilarities divided by the cluster size) (\pkg{fpc})
#'  - `"g2"` Goodman and Kruskal's Gamma coefficient (\pkg{fpc})
#'  - `"pearsongamma"` correlation between distances and a 0-1-vector where 0
#'     means same cluster, 1 means different clusters (\pkg{fpc})
#'  - `"dunn"` Dunn index (minimum separation / maximum diameter) (\pkg{fpc})
#'  - `"dunn2"` minimum average dissimilarity between two cluster /
#'     maximum average within cluster dissimilarity (\pkg{fpc})
#'  - `"entropy"` entropy of the distribution of cluster memberships (\pkg{fpc})
#'  - `"wb.ratio"` average.within/average.between (\pkg{fpc})
#'
#' The following external evaluation measures are available:
#'
#'  - `"precision"`, `"recall"`, `"F1"` F1.  A true positive (TP)
#'    decision assigns two points in the same true cluster also to the same
#'    cluster, a true negative (TN) decision assigns two points from two different
#'    true clusters to two different clusters.  A false positive (FP) decision
#'    assigns two points from the same true cluster to two different clusters.  A
#'    false negative (FN) decision assigns two points from the same true cluster
#'    to different clusters.
#'
#'    `precision = TP / (TP + FP)`
#'
#'    `recall = TP / (TP + FN)`
#'
#'    The F1 measure is the harmonic mean of precision and recall.
#' - `"purity"` Average purity of clusters. The purity of each cluster
#'   is the proportion of the points of the majority true group assigned to it
#'   (see Cao et al. (2006)).
#'  - `"classPurity"` (of real clusters; see Wan et al (2009)).
#'  - `"fpr"` false positive rate.
#'  - `"Euclidean"` Euclidean dissimilarity of the memberships (see
#'     Dimitriadou, Weingessel and Hornik (2002)) (\pkg{clue})
#'  - `"Manhattan"` Manhattan dissimilarity of the memberships (\pkg{clue})
#'  - `"Rand"` Rand index (see Rand (1971)) (\pkg{clue})
#'  - `"cRand"` Adjusted Rand index (see Hubert and Arabie (1985)) (\pkg{clue})
#'  - `"NMI"` Normalized Mutual Information (see Strehl and Ghosh (2002)) (\pkg{clue})
#'  - `"KP"` Katz-Powell index (see Katz and Powell (1953)) (\pkg{clue})
#'  - `"angle"` maximal cosine of the angle between the agreements (\pkg{clue})
#'  -` "diag"` maximal co-classification rate (\pkg{clue})
#'  - `"FM"` Fowlkes and Mallows's index (see Fowlkes and Mallows (1983)) (\pkg{clue})
#'  - `"Jaccard"` Jaccard index (\pkg{clue})
#'  - `"PS"` Prediction Strength (see Tibshirani and Walter (2005)) (\pkg{clue}) %
#'  - `"corrected.rand"`  corrected Rand index (\pkg{fpc})
#'  - `"vi"` variation of information (VI) index (\pkg{fpc})
#'
#' Many measures are the average over all clusters. For example, purity is the
#' average purity over all clusters.
#'
#' For [DSC_Micro] objects, data points are assigned to micro-clusters and
#' then each micro-cluster is evaluated. For [DSC_Macro] objects, data
#' points by default (`assign = "micro"`) also assigned to micro-clusters,
#' but these assignments are translated to macro-clusters. The evaluation is
#' here done for macro-clusters. This is important when macro-clustering is
#' done with algorithms which do not create spherical clusters (e.g,
#' hierarchical clustering with single-linkage or DBSCAN) and this assignment
#' to the macro-clusters directly (i.e., their center) does not make sense.
#'
#' Using `type` and `assign`, the user can select how to assign data
#' points and ad what level (micro or macro) to evaluate.
#'
#' `evaluate_cluster()` is used to evaluate an evolving data stream using
#' the method described by Wan et al. (2009). Of the `n` data points
#' `horizon` many points are clustered and then the evaluation measure is
#' calculated on the same data points. The idea is to find out if the
#' clustering algorithm was able to adapt to the changing stream.
#'
#' **Custom Evaluation Measures**
#'
#' The parameter `callbacks` can be supplied with a named list with
#' functions with the signature `function(actual, predict, points, centers, dsc)`
#' as elements. See the Examples sections for details.
#'
#' @family DSC
#' @family evaluation
#'
#' @name evaluate.DSC
#'
#' @param object The [DSC] object that the evaluation measure is being requested
#' from.
#' @param dsd The [DSD] object that holds the initial training data for the DSC.
#' @param measure Evaluation measure(s) to use. If missing then all available
#' measures are returned.
#' @param n The number of data points being requested.
#' @param type Use micro- or macro-clusters for evaluation. Auto used the class
#' of [DSC] to decide.
#' @param assign Assign points to micro or macro-clusters?
#' @param assignmentMethod How are points assigned to clusters for evaluation
#' (see [stream::predict()])?
#' @param horizon Evaluation is done using horizon many previous points (see
#' detail section).
#' @param verbose logical; Report progress?
#' @param excludeNoise logical; Should noise points in the data stream be excluded from
#'   the calculation?
#' @param callbacks A named list of functions to calculate custom evaluation measures.
#' @param ... Unused arguments are ignored.
#' @return `evaluate` returns an object of class `stream_eval` which
#' is a numeric vector of the values of the requested measures and two
#' attributes, `"type"` and `"assign"`, to see at what level the
#' evaluation was done.
#' @author Michael Hahsler, Matthew Bolanos, John Forrest, and Dalibor Krleža
#' @seealso [cluster::silhouette()], [clue:: cl_agreement()], and [fpc::cluster.stats()].
#' @references
#' Joao Gama, Raquel Sebastiao, Pedro Pereira Rodrigues (2013). On
#' evaluating stream learning algorithms. _Machine Learning,_ March 2013,
#' Volume 90, Issue 3, pp 317-346.
#'
#' F. Cao, M. Ester, W. Qian, A. Zhou (2006). Density-Based Clustering over an
#' Evolving Data Stream with Noise.
#' _Proceeding of the 2006 SIAM Conference on Data Mining,_ 326-337.
#'
#' E. Dimitriadou, A. Weingessel and K. Hornik (2002).  A combination scheme
#' for fuzzy clustering.
#' _International Journal of Pattern Recognition and Artificial Intelligence,_
#' 16, 901-912.
#'
#' E. B. Fowlkes and C. L. Mallows (1983).  A method for comparing two
#' hierarchical clusterings.
#' _Journal of the American Statistical Association,_ 78, 553-569.
#'
#' L. Hubert and P. Arabie (1985).  Comparing partitions.
#' _Journal of Classification,_ 2, 193-218.
#'
#' W. M. Rand (1971).  Objective criteria for the evaluation of clustering
#' methods.  _Journal of the American Statistical Association,_ 66,
#' 846-850.
#'
#' L. Katz and J. H. Powell (1953).  A proposed index of the conformity of one
#' sociometric measurement to another. _Psychometrika,_ 18, 249-256.
#'
#' A. Strehl and J. Ghosh (2002).  Cluster ensembles - A knowledge reuse
#' framework for combining multiple partitions.
#' _Journal of Machine Learning Research,_ 3, 583-617.
#'
#' R. Tibshirani and G. Walter (2005).  Cluster validation by Prediction
#' Strength. _Journal of Computational and Graphical Statistics,_ 14/3,
#' 511-528.
#'
#' L Wan, W.K. Ng, X.H. Dang, P.S. Yu and K. Zhang (2009). Density-Based
#' Clustering of Data Streams at Multiple Resolutions, _ACM Transactions
#' on Knowledge Discovery from Data,_ 3(3).
#'
#' D. Krleža, B. Vrdoljak, and M. Brčić (2020). Statistical Hierarchical
#' Clustering Algorithm for Outlier Detection in Evolving Data Streams,
#' _Springer Machine Learning_.
#' @examples
#' # Example 1: Static Evaluation
#' set.seed(0)
#' stream <- DSD_Gaussians(k = 3, d = 2)
#'
#' dstream <- DSC_DStream(gridsize = 0.05, Cm = 1.5)
#' update(dstream, stream, 500)
#' plot(dstream, stream)
#'
#' # Evaluate the micro-clusters in the clustering
#' # Note: we use here only n = 100 points for evaluation to speed up execution
#' evaluate_static(dstream, stream, n = 100)
#'
#' evaluate_static(dstream, stream,
#'   measure = c("numMicro", "numMacro", "purity", "crand", "SSQ"),
#'   n = 100)
#'
#' # DStream also provides macro clusters. Evaluate macro clusters with type = "macro"
#' # Note that SSQ and cRand increase.
#' plot(dstream, stream, type = "macro")
#' evaluate_static(dstream, stream, type = "macro",
#'   measure = c("numMicro", "numMacro", "purity", "crand", "SSQ"),
#'   n = 100)
#'
#' # Points are by default assigned to micro clusters using the method
#' # specified for the clustering algorithm.
#' # However, points can also be assigned to the closest macro-cluster using
#' # assign = "macro".
#' evaluate_static(dstream, stream, type = "macro", assign = "macro",
#'   measure = c("numMicro", "numMacro", "purity", "crand", "SSQ"),
#'   n = 100)
#'
#' # Example 2: Evaluate with Noise/Outliers
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
#' dstream <- DSC_DStream(gridsize = 0.05, Cm = 1.5)
#' update(dstream, stream, 500)
#'
#' # For cRand, noise is its own group, for SSQ, actual noise is always
#' # excluded.
#' plot(dstream, stream, 500)
#' evaluate_static(dstream, stream, n = 100,
#'   measure = c("numPoints", "noisePredicted", "noiseActual",
#'     "noisePrecision", "outlierJaccard", "cRand", "SSQ"))
#'
#' # Note that if noise is excluded, the number of used points is reduced.
#' evaluate_static(dstream, stream, n = 100,
#'   measure = c("numPoints", "noisePredicted", "noiseActual",
#'     "noisePrecision", "outlierJaccard", "cRand", "SSQ"), excludeNoise = TRUE)
#'
#'
#' # Example 3: Evaluate an evolving data stream
#' stream <- DSD_Benchmark(1)
#' dstream <- DSC_DStream(gridsize = 0.05, lambda = 0.1)
#'
#' evaluate_stream(dstream, stream, type = "macro", assign = "micro",
#'   measure = c("numMicro", "numMacro", "purity", "cRand"),
#'   n = 600, horizon = 100)
#'
#' if (interactive()){
#' # animate the clustering process
#' reset_stream(stream)
#' dstream <- DSC_DStream(gridsize = 0.05, lambda = 0.1)
#' animate_cluster(dstream, stream, horizon = 100, n = 5000,
#'   measure = "cRand", type = "macro", assign = "micro",
#'   plot.args = list(type = "both", xlim = c(0,1), ylim = c(0,1)))
#' }
#'
#' # Example 4: Add a custom measure as a callback
#' callbacks <- list(
#'    noisePercentage = function(actual, predict, points, centers, dsc) {
#'      sum(actual == 0L) / length(actual)
#'    },
#'    noiseFN = function(actual, predict, points, centers, dsc) {
#'      sum(actual == 0L & predict != 0L)
#'    },
#'    noiseFP = function(actual, predict, points, centers, dsc) {
#'      sum(actual != 0L & predict == 0L)
#'    }
#'  )
#'
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = .2)
#' dstream <- DSC_DStream(gridsize = 0.05, Cm = 1.5)
#' update(dstream, stream, 500)
#'
#' evaluate_static(dstream, stream,
#'   measure = c("numPoints", "noiseActual", "noisePredicted",
#'     "noisePercentage", "noiseFN", "noiseFP"),
#'   callbacks = callbacks, n = 100)
#'
#' evaluate_static(dstream, stream, callbacks = callbacks)
#' @export
evaluate_static.DSC <-
  function (object,
    dsd,
    measure,
    n = 100,
    type = c("auto", "micro", "macro"),
    assign = "micro",
    assignmentMethod = c("auto", "model", "nn"),
    excludeNoise = FALSE,
    callbacks = list(),
    ...) {
    type <- get_type(object, type)
    assignmentMethod <- match.arg(assignmentMethod)

    ## get points
    points <- get_points(dsd, n, info = TRUE)
    actual <- points[[".class"]]
    points <- remove_info(points)

    ## find all applicable measures.
    m <- c(measures_builtin_int, measures_fpc_int)

    # for external measures we need actual info from the stream.
    if (!is.null(actual))
      m <- c(m, measures_builtin_ext, measures_fpc_ext)

    # add callbacks
    m <- c(m, names(callbacks))

    if (missing(measure) || is.null(measure))
      measure <- m
    else {
      matchm <- pmatch(tolower(measure), tolower(m))

      if (any(is.na(matchm)))
        stop("Invalid or not applicable measures (may need class information): ",
          paste(measure[is.na(matchm)], collapse = ', '))

      measure <- m[matchm]
    }

    ## exclude noise?
    if(excludeNoise) {
      points <- points[!is.na(actual), , drop = FALSE]
      actual <- actual[!is.na(actual)]
    }

    # nothing left
    if (nrow(points) < 1L)
      return(data.frame(matrix(
        NA_real_,
        nrow = 1L,
        ncol = length(measure),
        dimnames = list(row = NULL, col = measure)
      )))

    ## assign points
    pred <-
      predict(object, points, type = assign, method = assignmentMethod, ...)
    pred <- pred[[".class"]]
    #print(table(actual, pred))

    ## translate micro to macro cluster ids if necessary
    if (type == "macro" &&
        assign == "micro")
      pred <- microToMacro(object, pred)
    else if (type != assign)
      stop("type and assign are not compatible!")

    centers <- get_centers(object, type = type)

    # treat noise
    pred[is.na(pred)] <- 0L
    if (!is.null(actual))
      actual[is.na(actual)] <- 0L


    res_buildin <- evaluate_buildin(
      measure[measure %in% c(measures_builtin_int, measures_builtin_ext)],
      actual,
      pred,
      points,
      centers,
      object)

    res_fpc <- evaluate_fpc(
      measure[measure %in% c(measures_fpc_int, measures_fpc_ext)],
      actual,
      pred,
      points,
      centers,
      object)

    res_callbacks <- evaluate_callbacks(
      measure[measure %in% c(names(callbacks))],
      actual,
      pred,
      points,
      centers,
      object,
      callbacks)

    res_all <- c(res_buildin, res_fpc, res_callbacks)[measure]

    structure(res_all,
      type = type,
      assign = assign,
      class = "stream_eval")
  }

## evaluate during clustering
## uses single-fold prequential error estimate (eval and then learn the data)
#' @rdname evaluate.DSC
#' @export
evaluate_stream.DSC <-
  function(object,
    dsd,
    measure,
    n = 1000,
    horizon = 100,
    type = c("auto", "micro", "macro"),
    assign = "micro",
    assignmentMethod =  c("auto", "model", "nn"),
    excludeNoise = FALSE,
    callbacks = NULL,
    ...,
    verbose = FALSE) {
    type <- get_type(object, type)

    rounds <- n %/% horizon

    evaluation <-
      data.frame(points = seq(
        from = 0,
        by = horizon,
        length.out = rounds
      ))
    for (m in measure)
      evaluation[[m]] <- NA_real_

    for (i in seq(rounds)) {
      d <- DSD_Memory(dsd, n = horizon, loop = FALSE)

      ## evaluate first
      reset_stream(d)

      evaluation[i, -1L] <-
        evaluate_static(
          object,
          d,
          measure,
          horizon,
          ### this is n
          type,
          assign,
          assignmentMethod,
          excludeNoise = excludeNoise,
          callbacks,
          ...
        )

      ## then update the model
      reset_stream(d)
      update(object, d, n = horizon)

      if (verbose)
        print(evaluation[i, ])
    }

    evaluation
  }




#' @export
print.stream_eval <-  function(x, ...) {
  cat("Evaluation results for ",
    attr(x, "type"),
    "-clusters.\n",
    sep = "")
  cat("Points were assigned to ",
    attr(x, "assign"),
    "-clusters.\n\n",
    sep = "")
  print(unclass(x))
}


# buildin measures
evaluate_buildin <-
  function(measure,
    actual,
    predict,
    points,
    centers,
    dsc) {

    # drop unknown measures
    measure <- measure[measure %in% c(measures_builtin_int, measures_builtin_ext)]
    if (is.null(actual) && any(measure %in% measures_builtin_ext))
      stop("External evaluation measure(s) ",
        paste(measure[measure %in% measures_builtin_ext], collapse = ", "),
        " not available for streams without cluster labels!")

    if(length(measure) < 1L)
      return(NULL)

    sapply(measure,
      evaluate_buildin_impl,
      actual,
      predict,
      points,
      centers,
      dsc)
  }


evaluate_buildin_impl <-
  function(measure,
    actual,
    predict,
    points,
    centers,
    dsc)
    switch(
      measure,

      ### internal
      numPoints = length(predict),
      numMicroClusters	= numClusters(dsc, "micro"),
      numMacroClusters	= numClusters(dsc, "macro"),

      noisePredicted	= sum(predict == 0L),

      SSQ  	     = ssq(points, actual, predict, centers),
      silhouette = silhouette(points, actual, predict),

      ### external
      numClasses = length(unique(actual)),

      noiseActual	    = sum(actual == 0L),
      noisePrecision	= sum(predict == 0L &
          actual == 0L) / sum(predict == 0L),
      outlierJaccard = outlierJaccard(predict,
        actual),

      precision	 = precision(actual, predict),
      recall	   = recall(actual, predict),
      F1		     = f1(actual, predict),

      Euclidean	 = agreement(predict, actual, "euclidean"),
      Manhattan	 = agreement(predict, actual, "manhattan"),
      Rand	     = agreement(predict, actual, "rand"),
      cRand	     = agreement(predict, actual, "crand"),
      NMI		     = agreement(predict, actual, "NMI"),
      KP		     = agreement(predict, actual, "KP"),
      angle	     = agreement(predict, actual, "angle"),
      diag	     = agreement(predict, actual, "diag"),
      FM		     = agreement(predict, actual, "FM"),
      Jaccard	   = agreement(predict, actual, "jaccard"),
      #purity	    = agreement(predict, actual, "purity"),
      PS		     = agreement(predict, actual, "PS"),
      purity     = purity(predict, actual)
      #classPurity	    = classPurity(actual, predict),
    )

measures_builtin_int <- c(
  "numPoints",
  "numMicroClusters",
  "numMacroClusters",

  "noisePredicted",

  "SSQ",
  "silhouette"
)

measures_builtin_ext  <-  c(
  "numClasses",

  "noiseActual",
  "noisePrecision",
  "outlierJaccard",

  "precision",
  "recall",
  "F1",
  "purity",
  #"fpr",
  #"classPurity",
  "Euclidean",
  "Manhattan",
  "Rand",
  "cRand",
  "NMI",
  "KP",
  "angle",
  "diag",
  "FM",
  "Jaccard",
  "PS"
)

# measures provided by fpc need special preprocessing and are done in a single call
evaluate_fpc <-
  function(
    measure,
    actual,
    predict,
    points,
    centers,
    dsc) {

    if (length(measure) > 1L) {
      # drop unknown measures
      measure <- measure[measure %in% c(measures_fpc_int, measures_fpc_ext)]
      if (is.null(actual) && any(measure %in% measures_fpc_ext))
        stop("External evaluation measure(s) ",
          paste(measure[measure %in% measures_fpc_ext], collapse = ", "),
          " not available for streams without cluster labels!")
    }

    if(length(measure) < 1L)
      return(NULL)

    ## we renumber so we have no missing cluster ID (noise is now cluster id 1)
    if(!is.null(actual))
      actual <- match(actual, unique(sort(actual)))
    predict <- match(predict, unique(sort(predict)))

    e <- fpc::cluster.stats(
      d = dist(points),
      clustering = predict,
      alt.clustering = actual,
      noisecluster = FALSE,
      silhouette = FALSE,
      G2 = TRUE,
      G3 = FALSE,
      wgap = FALSE,
      sepindex = FALSE,
      sepprob = 0.1,
      sepwithnoise = FALSE,
      compareonly = FALSE,
      aggregateonly = TRUE
    )

    unlist(e)[measure]
  }

measures_fpc_int  <- c(
  "average.between",
  "average.within",
  "max.diameter",
  "min.separation",
  "ave.within.cluster.ss",
  "g2",
  "pearsongamma",
  "dunn",
  "dunn2",
  "entropy",
  "wb.ratio"
)

measures_fpc_ext  <- c(# "corrected.rand",
  "vi")


# callbacks
evaluate_callbacks <-
  function(measure,
    actual,
    predict,
    points,
    centers,
    dsc,
    callbacks) {

    measure <- measure[measure %in% names(callbacks)]

    if(length(measure) < 1L)
      return(NULL)

    sapply(callbacks, FUN = function(cb) cb(actual,
      predict,
      points,
      centers,
      dsc))

  }

# implementation of individual measures

numClusters <- function(dsc, type) {
  if (is(try(n <-
      nclusters(dsc, type = type),
    silent = TRUE)
    , "try-error"))
    NA_integer_
  else
    n
}


## compare pairs of points
## http://stats.stackexchange.com/questions/15158/precision-and-recall-for-clustering
## test:
# actual  <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3)
# predict <- c(1,1,1,1,1,2,3,3,1,2,2,2,2,2,3,3,3)
# TOTAL = 136
# P = 40
# TP = 20
# FP = 20
# FN =24
# TN = 72

# recall TP/(TP+FN)
recall <- function(actual, predict) {
  conf <- table(predict, actual)
  #TOTAL <- length(actual)*(length(actual)-1)/2
  ## TP+FP
  #  P <- sum(choose(table(predict), 2))
  ## TP
  TP <- sum(choose(conf, 2))

  #N <- TOTAL - P

  FN <- sum(sapply(
    1:(nrow(conf) - 1L),
    FUN = function(i)
      conf[i, ] * colSums(conf[-(1:i), , drop = FALSE])
  ))

  #TN <- N - FN

  TP / (TP + FN)
}

# precision TP/(TP+FP)
precision <- function(actual, predict) {
  ## TP+FP
  P <- sum(choose(table(predict), 2))
  ## TP
  TP <- sum(choose(table(predict, actual), 2))

  TP / P
}

f1 <- function(actual, predict) {
  precision <- precision(actual, predict)
  recall <- recall(actual, predict)
  (2 * precision * recall) / (precision + recall)
}

purity <- function(actual, predict) {
  conf <- table(predict, actual)
  mean(colMax(conf) / colSums(conf))
}


## helper
colMax <- function(x, which = FALSE) {
  if (!which)
    apply(
      x,
      2,
      FUN = function(y)
        max(y)
    )
  else {
    apply(
      x,
      2,
      FUN = function(y)
        which.max(y)
    )
  }
}

rowMax <- function(x, which = FALSE) {
  if (!which)
    apply(
      x,
      1,
      FUN = function(y)
        max(y)
    )
  else {
    apply(
      x,
      1,
      FUN = function(y)
        which.max(y)
    )
  }
}

# as defined in Density-Based Clustering of Data Streams at
# Multiple Resolutions by Wan et al
classPurity <- function(actual, predict) {
  confusion <- table(actual, predict)
  mean(rowMax(confusion) / rowSums(confusion))
}

ssq <- function(points, actual, predict, centers) {
  # SSQ to closest center and does not use actual noise points

  if (!is.null(actual))
    points <- points[actual != 0L, , drop = FALSE]

  assign_dist <- apply(dist(points, centers), 1, min)
  sum(assign_dist ^ 2)
}

silhouette <- function(points, actual, predict) {
  ## silhouette does not use noise points
  if (!is.null(actual))
    noise <- actual == 0L & predict == 0L
  else
    noise <- predict == 0L

  points <- points[!noise, , drop = FALSE]
  predict <- predict[!noise]

  mean(cluster::silhouette(predict, dist(points))[, "sil_width"])
}


## this would need package Matrix
#get_confusionMatrix <- function(d,c,predict) {
#	#Get the actual class
#	actual <- attr(d, "assignment")
#
#	actual[is.na(actual)]<- 0
#
#	if(0 %in% actual)
#		actual <- actual + 1
#
#	result <- cbind(actual,predict)
#	#Compute the sparse matrix
#	confusion <- sparseMatrix(i = result[,1],j = result[,2], x = 1)
#	confusion
#}

outlierJaccard <-
  function(predict,
    actual) {
    tp <- sum(actual == 0L & predict == 0L)
    fp <- sum(predict == 0L & actual != 0L)
    undet <- sum(predict != 0L & actual == 0L)  # this is fn

    tp / (tp + fp + undet)
  }
