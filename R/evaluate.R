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

#' Evaluate Clusterings
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
#' - `evaluate()` evaluates the current static clustering using new data.
#' - `evaluate_cluster()` evaluates the clustering process. The most commonly used method
#'   is _prequential error estimation_ (see Gama, Sebastiao and Rodrigues; 2013).  The data points
#'   in the horizon are first used to calculate the evaluation measure and then
#'   they are used for updating the cluster model.
#'
#' **Evaluation Measures**
#'
#' Many evaluation measures are available using
#' code from other packages including [cluster::silhouette()],
#' [clue:: cl_agreement()], and [fpc::cluster.stats()].
#'
#' The following information items are available:
#'
#'  - `"numMicroClusters"` number of micro-clusters
#'  - `"numMacroClusters"` number of macro-clusters
#'  - `"numClasses"` number of classes
#'
#' The following noise-related items are available:
#'
#'  - `"noisePredicted"` Number data points predicted as noise
#'  - `"noiseActual"` Number of data points which are actually noise
#'  - `"noisePrecision"` Precision of the predicting noise (i.e., number of
#'     correctly predicted noise points over the total number of points predicted
#'     as noise)
#'
#' The following internal evaluation measures are available:
#'  - `"SSQ"` within cluster sum of squares. Assigns each non-noise point to
#'     its nearest center from the clustering and calculates the sum of squares
#'  - `"silhouette"` average silhouette width (actual noise points
#'     which stay unassigned by the clustering algorithm are removed; regular
#'     points that are unassigned by the clustering algorithm will form their own
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
#'    precision = TP/(TP+FP)
#'
#'    recall = TP/(TP+FN)
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
#' The following outlier measures are available:
#'  - `"OutlierJaccard"` - A variant of the Jaccard index used to assess
#'     outlier detection accuracy (see Krleza et al (2020)).  Outlier Jaccard index
#'     is calculated as TP/(TP+FP+UNDETECTED).
#'
#' Outlier measures are taken as
#' external measures, and can be applied only for DSD that can mark outliers
#' (see [DSD_Gaussians]) and outlier detection clusterers that
#' inherits [DSOutlier] class.
#'
#' `evaluate_cluster()` is used to evaluate an evolving data stream using
#' the method described by Wan et al. (2009). Of the `n` data points
#' `horizon` many points are clustered and then the evaluation measure is
#' calculated on the same data points. The idea is to find out if the
#' clustering algorithm was able to adapt to the changing stream.
#'
#' **Custom Evaluation Measures**
#'
#' The parameter `callbacks` can
#' be used to add user-defined measure
#' calculations. The user can define a measure by creating a sub-class of `EvalCallback`
#' and adding it to a list of callbacks (see [EvalCallback] for details).
#' At the end of each evaluation, the set of `callbacks` are performed and the results
#' are reported.
#'
#' @family DSC
#' @family evaluation
#'
#' @param dsc The [DSC] object that the evaluation measure is being requested
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
#' @param verbose Report progress?
#' @param noise How to handle noise points in the data. Options are to treat as
#' a separate class (default) or to exclude them from evaluation.
#' @param callbacks A list of [EvalCallback] objects, invoked when
#' measurement is calculated.
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
#' set.seed(0)
#' stream <- DSD_Gaussians(k = 3, d = 2)
#'
#' dstream <- DSC_DStream(gridsize = 0.05, Cm = 1.5)
#' update(dstream, stream, 500)
#' plot(dstream, stream)
#'
#' # Evaluate the micro-clusters in the clustering
#' # Note: we use here only n = 100 points for evaluation to speed up execution
#' evaluate(dstream, stream, n = 100)
#'
#' evaluate(dstream, stream,
#'   measure=c("numMicro", "numMacro", "purity", "crand", "SSQ"),
#'   n = 100)
#'
#' # DStream also provides macro clusters. Evaluate macro clusters with type="macro"
#' plot(dstream, stream, type = "macro")
#' evaluate(dstream, stream, type ="macro",
#'   measure = c("numMicro","numMacro","purity","crand", "SSQ"),
#'   n = 100)
#'
#' # Points are by default assigned to the closest micro clusters for evaluation.
#' # However, points can also be assigned to the closest macro-cluster using
#' # assign = "macro".
#' evaluate(dstream, stream, type = "macro", assign = "macro",
#'   measure = c("numMicro", "numMacro", "purity", "crand", "SSQ"),
#'   n = 100)
#'
#' # Evaluate an evolving data stream
#' stream <- DSD_Benchmark(1)
#' dstream <- DSC_DStream(gridsize = 0.05, lambda = 0.1)
#'
#' evaluate_cluster(dstream, stream, type = "macro", assign = "micro",
#'   measure=c("numMicro", "numMacro", "purity", "crand"),
#'   n = 600, horizon = 100)
#'
#' # animate the clustering process
#' if (interactive()){
#' reset_stream(stream)
#' dstream <- DSC_DStream(gridsize = 0.05, lambda = 0.1)
#' animate_cluster(dstream, stream, horizon = 100, n = 5000,
#'   measure = c("crand"), type = "macro", assign = "micro",
#'   plot.args = list(type = "both", xlim = c(0,1), ylim = c(0,1)))
#' }
#' @export
evaluate <- function(...)
  UseMethod("evaluate")

#' @rdname evaluate
#' @export
evaluate.DSC <-
  function (dsc,
    dsd,
    measure,
    callbacks = list(),
    n = 100,
    type = c("auto", "micro", "macro"),
    assign = "micro",
    assignmentMethod = c("auto", "model", "nn"),
    noise = c("class", "exclude"),
    ...) {
    assignmentMethod <- match.arg(assignmentMethod)
    noise <- match.arg(noise)
    type <- get_type(dsc, type)

    if (!is.list(callbacks))
      callbacks <- list(callbacks)

    callbacks$default <- DefaultEvalCallback()

    if (!all(sapply(callbacks, is, "EvalCallback")))
      stop("Callbacks must comprise a list of 'EvalCallback' objects!")

    ## get points
    points <- get_points(dsd, n, info = TRUE)
    actual <- points[[".class"]]
    outliers_actual <- points[[".outliers"]]

    if (is.null(actual))
      stop("The stream (dsd) does not provide true class/cluster labels in '.class'.")

    if (is.null(outliers_actual) && any(is.na(actual)))
      outliers_actual <- is.na(actual)

    if (all(is.na(actual)))
      warning(
        "All points used for evaluation have a missing class/cluster label. Evaluation results will not be useful!"
      )
    points <- remove_info(points)

    # find all applicable default measures
    m <-
      c(sapply(callbacks, function(cb_obj)
        cb_obj$env$internal_measures))

    if (!is.null(actual))
      m <- append(m, c(
        sapply(callbacks, function(cb_obj)
          cb_obj$env$external_measures)
      ))

    if (!is.null(outliers_actual))
      m <- append(m, c(sapply(callbacks, function(cb_obj)
        cb_obj$env$outlier_measures)))

    m <- unlist(m)

    if (missing(measure) || is.null(measure))
      measure <- m
    else {
      matchm <- pmatch(tolower(measure), tolower(m))

      if (any(is.na(matchm)))
        stop("Invalid or not applicable measure: ", paste(measure[is.na(matchm)], collapse = ', '))

      m <- m[matchm]
    }

    ## assign points
    pred <-
      predict(dsc, points, type = assign, method = assignmentMethod, ...)

    # if we have an outlier detecting clusterer, assignment must have returned both predicted
    # classes and outlier flags
    predict_outliers <- pred[[".outliers"]]
    predict_outliers_corrid <- pred[[".outliers_corrid"]]

    pred <- pred[[".class"]]
    #print(table(actual,pred))

    if(is.null(predict_outliers))
      predict_outliers <- is.na(pred)

    ## translate micro to macro cluster ids if necessary
    if (type == "macro" &&
        assign == "micro")
      pred <- microToMacro(dsc, pred)
    else if (type != assign)
      stop("type and assign are not compatible!")
    #print(table(pred,actual))

    ## predicted noise is still its own class?
    pred[is.na(pred)] <- 0L

    centers <- get_centers(dsc, type = type)

    e <- c()
    for (x in callbacks) {
      m_tmp <-
        x$env$all_measures[pmatch(tolower(m), tolower(x$env$all_measures))]
      m_tmp <- m_tmp[!is.na(m_tmp)]
      ec <-
        evaluate_callback(
          x,
          dsc,
          m_tmp,
          points,
          actual,
          pred,
          outliers_actual,
          predict_outliers,
          predict_outliers_corrid,
          centers,
          noise
        )
      e <- c(e, ec)
    }

    structure(e,
      type = type,
      assign = assign,
      class = "stream_eval")
  }

## internal measures from package fpc
.eval_measures_fpc_int  <- c(
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

## external measures from package fpc
.eval_measures_fpc_ext  <- c(# "corrected.rand",
  "vi")

## this also contains info and noise
.eval_measures_int  <- c(
  ## info
  "numMicroClusters",
  "numMacroClusters",
  "numClasses",

  ## noise
  "noisePredicted",
  "noiseActual",
  "noisePrecision",

  ## internal
  "SSQ",
  "silhouette"
)

.eval_measures_ext  <- c(
  # external
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

.eval_measures_outlier <-
  c(#inherits all Jaccard properties, it is an external measure
    "OutlierJaccard")

.all_measures <- c(
  .eval_measures_int,
  .eval_measures_ext,
  .eval_measures_fpc_int,
  .eval_measures_fpc_ext,
  .eval_measures_outlier
)


#' Abstract Class for Evaluation Callbacks
#'
#' The abstract class for all evaluation callbacks. Cannot be instantiated.
#' Must be inherited. Evaluation is the process of the clustering quality
#' assessment. This assessment can include clustering results, as well as the
#' clustering process, e.g., duration, spatial query performance, and similar.
#' The \pkg{stream} package has some measurements (see [evaluate()] for
#' details) already implemented. All other measurements can be externally
#' implemented without need to extend the \pkg{stream} package, by using
#' callbacks.
#'
#' @param ... further arguments.
#' @name EvalCallback-class
#' @aliases EvalCallback evaluate_callback
#' @docType class
#' @section Fields:
#' \describe{
#'   \item{all_measures}{ A list of all measures this
#' object contributes to the evaluation. Union of all callback measures defines
#' measures the end-user can use. }
#'   \item{internal_measures}{ A list of
#' internal measures. A subset of `all_measures`. }
#'   \item{external_measures}{ A list of external measures. A subset of
#' `all_measures`. }
#'   \item{outlier_measures}{ A list of outlier measures.
#' A subset of `all_measures`. }
#' }
#' @author Dalibor Krleža
#' @examples
#' # 1. define a class constructor to hold the evaluation information
#' LWP_Callback <- function() {
#'   env <- environment()
#'   all_measures <- c("LowestWeightPercentage")
#'   internal_measures <- c()
#'   external_measures <- all_measures
#'   outlier_measures <- c()
#'   structure(list(description = "Custom evaluation callback",
#'            env = environment()),
#'            class = c("LWP_Callback", "EvalCallback"))
#' }
#'
#' # 2. define the evaluation method for the class and calculate the
#' #    custom measure if it is requested in measure.
#' evaluate_callback.LWP_Callback <- function(cb_obj, dsc, measure, points,
#'                                              actual, predict, outliers,
#'                                              predict_outliers,
#'                                              predict_outliers_corrid,
#'                                              centers, noise) {
#'     r <- list()
#'     if("LowestWeightPercentage" %in% measure)
#'         r$LowestWeightPercentage = min(get_weights(dsc)) / sum(get_weights(dsc))
#'     r
#' }
#'
#' # 3. evaluate with callbacks
#' stream <- DSD_Gaussians(k = 3, d = 2, p = c(0.2, 0.4, 0.4))
#' km <- DSC_Kmeans(3)
#' update(km, stream, n=500)
#' evaluate(km, stream, type="macro", n=500,
#'                         measure = c("crand","LowestWeightPercentage"),
#'                         callbacks = list(cc = LWP_Callback()))
EvalCallback <-
  function(...)
    stop("EvalCallback is an abstract class and cannot be instantiated!")

evaluate_callback <-
  function(cb_obj,
    dsc,
    measure,
    points,
    actual,
    predict,
    outliers,
    predict_outliers,
    predict_outliers_corrid,
    centers,
    noise,
    ...)
UseMethod("evaluate_callback")

DefaultEvalCallback <- function() {
  env <- environment()
  all_measures <- .all_measures
  internal_measures <- c(.eval_measures_int, .eval_measures_fpc_int)
  external_measures <-
    c(.eval_measures_ext,
      .eval_measures_fpc_ext)
  outlier_measures <- c(.eval_measures_outlier)
  this <-
    list(description = "Default evaluation callback", env = environment())
  class(this) <- c("DefaultEvalCallback", "EvalCallback")
  this
}

evaluate_callback.DefaultEvalCallback <-
  function(cb_obj,
    dsc,
    measure,
    points,
    actual,
    predict,
    outliers,
    predict_outliers,
    predict_outliers_corrid,
    centers,
    noise) {
    ## no centers available
    if (nrow(centers) < 1) {
      #warning("No centers available!")
      e <- rep.int(NA_real_, length(measure))
      e[measure %in% c("numMicroClusters", "numMacroClusters")] <- 0
      names(e) <- measure
      return(e)
    }

    fpc <-
      measure %in% c(.eval_measures_fpc_int, .eval_measures_fpc_ext)
    if (any(fpc)) {
      actual_fpc <- actual
      predict_fpc <- predict
      points_fpc <- points

      ## deal with noise
      withnoise <- FALSE
      if (noise == "class") {
        ## noise in fpc has the highest index
        if (!is.null(actual_fpc))
          actual_fpc[is.na(actual_fpc)] <-
            max(actual_fpc, na.rm = TRUE)
        predict_fpc[is.na(predict_fpc)] <-
          max(predict_fpc, na.rm = TRUE)

      } else if (noise == "exclude") {
        ## remove all actual noise points
        if (!is.null(actual_fpc)) {
          nsp <- is.na(actual_fpc)
          actual_fpc <- actual_fpc[!nsp]
          predict_fpc <- predict_fpc[!nsp]
          predict_fpc[is.na(predict_fpc)] <-
            max(predict_fpc, na.rm = TRUE)
          points_fpc <- points_fpc[!nsp, , drop = FALSE]
        }
      } else
        stop("Unknown noise treatment!")

      ## we also renumber so we have no missing cluster ID
      actual_fpc <- match(actual_fpc, unique(sort(actual_fpc)))
      predict_fpc <- match(predict_fpc, unique(sort(predict_fpc)))

      e <- fpc::cluster.stats(
        d = dist(points_fpc),
        clustering = predict_fpc,
        alt.clustering = actual_fpc,
        noisecluster = TRUE,
        silhouette = FALSE,
        G2 = TRUE,
        G3 = FALSE,
        wgap = FALSE,
        sepindex = FALSE,
        sepprob = 0.1,
        sepwithnoise = withnoise,
        compareonly = FALSE,
        aggregateonly = TRUE
      )
      e <- unlist(e)
    } else
      e <- numeric()

    if (any(!fpc)) {
      ## deal with noise
      if (noise == "class") {
        ## noise it its own group with index 0: this works for external measures
        if (!is.null(actual))
          actual[is.na(actual)] <- 0L
        predict[is.na(predict)] <- 0L
      } else if (noise == "exclude") {
        ## remove all actual noise points
        if (!is.null(actual)) {
          nsp <- is.na(actual)
          actual <- actual[!nsp]
          predict <- predict[!nsp]
          points <- points[!nsp, , drop = FALSE]
        }
      } else
        stop("Unknown noise treatment!")

      v <- sapply(measure[!fpc],
        function(m)
          .evaluate(
            m,
            predict,
            actual,
            points,
            centers,
            outliers,
            predict_outliers,
            predict_outliers_corrid,
            dsc,
            cb_obj
          ))
      e <- c(e, v)
    }

    e <- e[measure]
    return(e)
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
  names <- names(x)
  x <- as.numeric(x)
  names(x) <- names
  print(x)
}


## evaluate during clustering
## uses single-fold prequential error estimate (eval and then learn the data)
#' @rdname evaluate
#' @export
evaluate_cluster <-
  function(dsc,
    dsd,
    measure,
    callbacks = NULL,
    n = 1000,
    type = c("auto", "micro", "macro"),
    assign = "micro",
    assignmentMethod =  c("auto", "model", "nn"),
    horizon = 100,
    verbose = FALSE,
    noise = c("class", "exclude"),
    ...) {
    if (is.null(callbacks))
      list(default = DefaultEvalCallback())

    callbacks <- .addMissingDefaultCallback(callbacks)

    if (is.null(callbacks) ||
        !is.list(callbacks) || length(callbacks) < 1)
      stop("Callbacks must comprise a list of objects")
    for (x in callbacks)
      if (!is.object(x) || !is(x, "EvalCallback"))
        stop("All callbacks must be derived from EvalCallback")

    rounds <- n %/% horizon
    measure <-
      c(sapply(callbacks, function(cb_obj)
        cb_obj$env$all_measures[pmatch(tolower(measure), tolower(cb_obj$env$all_measures))]))
    measure <- measure[!is.na(measure)]

    evaluation <-
      data.frame(points = seq(
        from = 1,
        by = horizon,
        length.out = rounds
      ))
    for (m in measure)
      evaluation[[m]] <- NA_real_

    for (i in 1:rounds) {
      d <- DSD_Memory(dsd, n = horizon, loop = FALSE)

      ## evaluate first
      reset_stream(d)

      r <-
        evaluate(dsc,
          d,
          measure,
          callbacks,
          horizon,
          type,
          assign,
          assignmentMethod,
          noise = noise,
          ...)
      evaluation[i,] <- c(i * horizon, r)

      ## then update the model
      reset_stream(d)
      update(dsc, d, n = horizon)

      if (verbose)
        print(evaluation[i,])
    }

    evaluation
  }

# work horse
.evaluate <-
  function(measure,
    predict,
    actual,
    points,
    centers,
    outliers,
    predict_outliers,
    predict_outliers_corrid,
    dsc,
    callback_obj) {
    if (is.null(actual) && !measure %in% .eval_measures_int)
      stop("Evaluation measure not available for streams without cluster labels!")

    res <- switch(
      measure,
      numMicroClusters	= if (is(try(n <-
          nclusters(dsc, type = "micro"),
        silent = TRUE)
        , "try-error"))
        NA_integer_
      else
        n,
      numMacroClusters	= if (is(try(n <-
          nclusters(dsc, type = "macro"),
        silent = TRUE)
        , "try-error"))
        NA_integer_
      else
        n,
      numClasses	      = numClasses(actual),

      noisePredicted	= sum(predict == 0L),
      noiseActual	    = sum(actual == 0L),
      noisePrecision	= sum(predict == 0L &
          actual == 0L) / sum(predict == 0L),

      SSQ  	     = ssq(points, actual, predict, centers),
      silhouette = silhouette(points, actual, predict),

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

      purity     = purity(predict, actual),
      #classPurity	    = classPurity(actual, predict),
      OutlierJaccard = outlierJaccard(
        predict,
        actual,
        outliers,
        predict_outliers,
        predict_outliers_corrid,
        dsc,
        callback_obj
      ),
    )

    res
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
      conf[i,] * colSums(conf[-(1:i), , drop = FALSE])
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

## FIXME: check!
# as defined in Density-Based Clustering of Data Streams at
# Multiple Resolutions by Wan et al
classPurity <- function(actual, predict) {
  confusion <- table(actual, predict)
  mean(rowMax(confusion) / rowSums(confusion))
}

numClusters <- function(centers) {
  nrow(centers)
}

numClasses <- function(actual) {
  length(unique(actual))
}

ssq <- function(points, actual, predict, centers) {
  #   ## ssq does not use actual and predicted noise points
  #   ## predicted noise points that are not actual noise points form their own
  #   ## cluster
  #   if(!is.null(actual)) noise <- actual==0 & predict==0
  #   else noise <- predict==0
  #
  #   points <- points[!noise,]
  #   predict <- predict[!noise]
  #   if(any(predict==0)) {
  #     warning("SSQ: ", sum(predict==0), " non-noise points were predicted noise incorrectly and form their own cluster.")
  #     centers <- rbind(centers, colMeans(points[predict==0,]))
  #     predict[predict==0] <- nrow(centers)
  #   }
  #
  #   ## points that are predicted as noise but are not are its own group!
  #
  #   #sum(apply(dist(points, centers), 1L , min)^2)
  #   d <- dist(points, centers)
  #   sum(sapply(1:length(predict), FUN=function(i) d[i,predict[i]])^2)

  ## do nn assignment of non noise points
  if (!is.null(actual))
    points <- points[actual != 0L,]

  assign_dist <- apply(dist(points, centers), 1, min)
  sum(assign_dist ^ 2)
}

silhouette <- function(points, actual, predict) {
  ## silhouette does not use noise points
  if (!is.null(actual))
    noise <- actual == 0 & predict == 0
  else
    noise <- predict == 0

  points <- points[!noise,]
  predict <- predict[!noise]

  #  if(any(predict==0)) warning("silhouette: ", sum(predict==0), " non-noise points were predicted noise incorrectly and form their own cluster.")

  ## points that are predicted as noise but are not are its own group!


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
    actual,
    outliers,
    predict_outliers,
    predict_outliers_corrid,
    dsc,
    callback_obj) {
    if (missing(dsc))
      warning("dsc is missing")
    if (is.null(callback_obj$env$copi))
      cumulative <-
        list(
          go = 0,
          tp = 0,
          fp = 0,
          undet = 0,
          tp_corrid = c(),
          fp_corrid = c(),
          undet_corrid = c()
        )
    else
      cumulative <- callback_obj$env$copi
    cumulative$go <-
      cumulative$go + sum(outliers) # the number of generated outliers
    if (missing(predict_outliers) ||
        is.null(predict_outliers) ||
        length(predict_outliers) != length(outliers)) {
      # we estimate whether the actual class resides in a predicted class that has only one appearance
      # this is for clusterers that have no explicit outlier recognition
      conf <- table(predict, actual) # coincidence matrix
      undet_outliers <-
        tmp_outliers <- actual[which(outliers == TRUE)]
      for (rindex in 1:nrow(conf)) {
        if (sum(conf[rindex,]) == 1) {
          # This is a detected outlier
          for (cindex in 1:ncol(conf)) {
            # pass over generated classes and check which one has this outlier
            if (conf[rindex, cindex] == 1 &&
                sum(conf[, cindex] == 1)) {
              # this is obviously one data instance in its own actual and predicted class
              actual_out <- colnames(conf)[[cindex]]
              if (actual_out %in% tmp_outliers) {
                undet_outliers <- undet_outliers[undet_outliers != actual_out]
                cumulative$tp <-
                  cumulative$tp + 1 # it was marked by the stream, therefore true positive
              } else
                cumulative$fp <-
                  cumulative$fp + 1 # it was NOT marked by the stream, therefore false positive
            }
          }
        }
      }
      cumulative$undet <- cumulative$undet + length(undet_outliers)
      if (cumulative$go > 0)
        cumulative$oji <-
        cumulative$tp / (cumulative$tp + cumulative$fp + cumulative$undet)
      else
        cumulative$oji <- 0.0
    } else {
      # this is matching for clusterers that have explicit outlier recognition
      act_out <- which(outliers)
      pred_out <- which(predict_outliers)
      cumulative$undet_corrid <-
        unique(c(cumulative$undet_corrid, predict_outliers_corrid[act_out[which(!act_out %in% pred_out)]]))
      cumulative$tp_corrid <-
        unique(c(cumulative$tp_corrid, predict_outliers_corrid[act_out[which(act_out %in% pred_out)]]))
      cumulative$fp_corrid <-
        unique(c(cumulative$fp_corrid, predict_outliers_corrid[pred_out[which(!pred_out %in% act_out)]]))
      if (!is.null(dsc$recheck_outliers) && dsc$recheck_outliers) {
        for (o_id in cumulative$tp_corrid)
          #[which(!cumulative$tp_corrid %in% p2)]) {
          if (!recheck_outlier(dsc, o_id)) {
            cumulative$tp_corrid <-
              cumulative$tp_corrid[cumulative$tp_corrid != o_id] # remove it from the TP
            cumulative$undet_corrid <-
              c(cumulative$undet_corrid, o_id) # but we add it to the undetected set
          }
        for (o_id in cumulative$fp_corrid)
          #[which(!cumulative$fp_corrid %in% p2)])
          if (!recheck_outlier(dsc, o_id))
            cumulative$fp_corrid <-
              cumulative$fp_corrid[cumulative$fp_corrid != o_id]
      }
      #message(paste("OJI TP=",length(cumulative$tp_corrid),"FP=",length(cumulative$fp_corrid),"UNDET=",length(cumulative$undet_corrid)))
      cumulative$oji <-
        length(cumulative$tp_corrid) / (
          length(cumulative$tp_corrid) + length(cumulative$fp_corrid) + length(cumulative$undet_corrid)
        )
    }
    callback_obj$env$copi <- cumulative
    cumulative$oji
  }

.addMissingDefaultCallback <-
  function(callbacks) {
    # we check and add default callback if missing
    default <- FALSE
    for (x in callbacks) {
      if (is(x, "DefaultEvalCallback"))
        default <- TRUE
    }
    if (!default)
      callbacks$default <- DefaultEvalCallback()
    callbacks
  }
