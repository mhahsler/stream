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

## internal measures from package fpc
.eval_measures_fpc_int  <- c(
  "average.between",
  "average.within",
  "max.diameter",
  "min.separation",
  "ave.within.cluster.ss",
  "g2", "pearsongamma",
  "dunn", "dunn2",
  "entropy", "wb.ratio"
)

## external measures from package fpc
.eval_measures_fpc_ext  <- c(
  # "corrected.rand",
  "vi"
)

## this also contains info and noise
.eval_measures_int  <- c(
  ## info
  "numMicroClusters", "numMacroClusters", "numClasses",

  ## noise
  "noisePredicted", "noiseActual", "noisePrecision",

  ## internal
  "SSQ",
  "silhouette"
)

.eval_measures_ext  <- c(
  # external
  "precision", "recall", "F1",
  "purity",
  #"fpr",
  #"classPurity",
  "Euclidean", "Manhattan", "Rand", "cRand",
  "NMI", "KP", "angle", "diag", "FM", "Jaccard", "PS"
)

.eval_measures_outlier <- c( #inherits all Jaccard properties, it is an external measure
  "OutlierJaccard"
)

.all_measures <- c(.eval_measures_int, .eval_measures_ext,
                   .eval_measures_fpc_int, .eval_measures_fpc_ext,
                   .eval_measures_outlier)

EvalCallback <- function(...) stop("EvalCallback is an abstract class and cannot be instantiated!")
evaluate_callback <- function(cb_obj, dsc, measure, points, actual, predict,
                              outliers, predict_outliers, predict_outliers_corrid,
                              centers, noise, ...) UseMethod("evaluate_callback")
evaluate_callback.default <- function(cb_obj, dsc, measure, points, actual, predict,
                                      outliers, predict_outliers, predict_outliers_corrid,
                                      centers, noise, ...) {
  stop(gettextf("evaluate_callback not implemented for class '%s'.",
                paste(class(cb_obj), collapse=", ")))
}
DefaultEvalCallback <- function() {
  env <- environment()
  all_measures <- .all_measures
  internal_measures <- c(.eval_measures_int, .eval_measures_fpc_int)
  external_measures <- c(.eval_measures_ext, .eval_measures_fpc_ext, .eval_measures_outlier)
  outlier_measures <- c(.eval_measures_outlier)
  this <- list(description = "Default evaluation callback", env = environment())
  class(this) <- c("DefaultEvalCallback", "EvalCallback")
  this
}
evaluate_callback.DefaultEvalCallback <- function(cb_obj, dsc, measure, points, actual, predict,
                                                  outliers, predict_outliers, predict_outliers_corrid,
                                                  centers, noise) {
  ## no centers available
  if(nrow(centers)<1) {
    #warning("No centers available!")
    e <- rep.int(NA_real_, length(measure))
    e[measure %in% c("numMicroClusters", "numMacroClusters")] <- 0
    names(e) <- measure
    return(e)
  }

  fpc <- measure %in% c(.eval_measures_fpc_int, .eval_measures_fpc_ext)
  if(any(fpc)) {
    actual_fpc <- actual
    predict_fpc <- predict
    points_fpc <- points

    ## deal with noise
    withnoise <- FALSE
    if(noise=="class") {
      ## noise in fpc has the highest index
      if(!is.null(actual_fpc))
        actual_fpc[is.na(actual_fpc)] <- max(actual_fpc, na.rm = TRUE)
      predict_fpc[is.na(predict_fpc)] <- max(predict_fpc, na.rm = TRUE)

    } else if(noise=="exclude") {
      ## remove all actual noise points
      if(!is.null(actual_fpc)) {
        nsp <- is.na(actual_fpc)
        actual_fpc <- actual_fpc[!nsp]
        predict_fpc <- predict_fpc[!nsp]
        predict_fpc[is.na(predict_fpc)] <- max(predict_fpc, na.rm = TRUE)
        points_fpc <- points_fpc[!nsp, , drop=FALSE]
      }
    } else stop("Unknown noise treatment!")

    ## we also renumber so we have no missing cluster ID
    actual_fpc <- match(actual_fpc, unique(sort(actual_fpc)))
    predict_fpc <- match(predict_fpc, unique(sort(predict_fpc)))

    e <- fpc::cluster.stats(
      d=dist(points_fpc),
      clustering=predict_fpc,
      alt.clustering = actual_fpc,
      noisecluster=TRUE,
      silhouette = FALSE,
      G2 = TRUE, G3 = FALSE,
      wgap=FALSE, sepindex=FALSE, sepprob=0.1,
      sepwithnoise=withnoise,
      compareonly = FALSE,
      aggregateonly = TRUE)
    e <- unlist(e)
  } else e <- numeric()

  if(any(!fpc)) {

    ## deal with noise
    if(noise=="class") {
      ## noise it its own group with index 0: this works for external measures
      if(!is.null(actual)) actual[is.na(actual)] <- 0L
      predict[is.na(predict)] <- 0L
    } else if(noise=="exclude") {
      ## remove all actual noise points
      if(!is.null(actual)) {
        nsp <- is.na(actual)
        actual <- actual[!nsp]
        predict <- predict[!nsp]
        points <- points[!nsp, , drop = FALSE]
      }
    } else stop("Unknown noise treatment!")

    v <- sapply(measure[!fpc],
                function(m) .evaluate(m, predict, actual, points, centers, outliers, predict_outliers,
                                      predict_outliers_corrid, dsc, cb_obj))
    e <- c(e, v)
  }

  e <- e[measure]
  return(e)
}


evaluate <- function (dsc, dsd, measure, n = 100, type=c("auto", "micro", "macro"),
                      assign="micro", assignmentMethod=c("auto","model", "nn"),
                      noise = c("class", "exclude"), ...) {
  .evaluate_with_callbacks(dsc, dsd, measure, list(default=DefaultEvalCallback()), n, type,
                           assign, assignmentMethod, noise)
}


evaluate_with_callbacks <- function (dsc, dsd, measure, callbacks=NULL, n = 100,
                                     type=c("auto", "micro", "macro"),
                                     assign="micro",
                                     assignmentMethod=c("auto","model", "nn"),
                                     noise = c("class", "exclude"), ...) {
  if(is.null(callbacks)) callbacks <- list()
  callbacks <- .addMissingDefaultCallback(callbacks)
  .evaluate_with_callbacks(dsc, dsd, measure, callbacks, n, type, assign,
                           assignmentMethod, noise)
}

.evaluate_with_callbacks <- function (dsc, dsd, measure, callbacks, n = 100,
                                      type=c("auto", "micro", "macro"),
                                      assign="micro",
                                      assignmentMethod=c("auto","model", "nn"),
                                      noise = c("class", "exclude"),
                                      ...) {
  if(is.null(callbacks) || !is.list(callbacks) || length(callbacks)<1)
    stop("Callbacks must comprise a list of objects")
  for(x in callbacks)
    if(!is.object(x) || !is(x, "EvalCallback"))
      stop("All callbacks must be derived from EvalCallback")

  assignmentMethod <- match.arg(assignmentMethod)
  noise <- match.arg(noise)
  type <- get_type(dsc, type)

  points <- get_points(dsd, n, cluster = TRUE, outlier = TRUE)
  actual <- attr(points, "cluster")
  outliers <- attr(points, "outlier")
  if(is.null(actual)) warning("the stream (dsd) does not provide true class/cluster labels.")
  if(all(is.na(actual))) warning("all points used for evaluation have a missing class/cluster label. Evaluation results will not be useful!")

  if(missing(measure) || is.null(measure)) {
    if(!is.null(actual)) m <- c(sapply(callbacks, function(cb_obj) cb_obj$env$all_measures))
    else m <- c(sapply(callbacks, function(cb_obj) cb_obj$env$internal_measures))
  } else m <- c(sapply(callbacks, function(cb_obj) cb_obj$env$all_measures[pmatch(tolower(measure), tolower(cb_obj$env$all_measures))]))
  m <- unname(m[!is.na(m)])

  if(any(is.na(m)))
    stop("Invalid measure: ", paste(measure[is.na(m)], collapse=', '))

  if(is.null(actual)) {
    .m_ext <- c(sapply(callbacks, function(cb_obj) cb_obj$env$external_measures))
    .m_ext <- unname(.m_ext[!is.na(.m_ext)])
    if(any(m %in% .m_ext))
      stop("External evaluation measures not available for streams without cluster labels!")
  }

  if(is.null(outliers)) {
    .m_out <- c(sapply(callbacks, function(cb_obj) cb_obj$env$outlier_measures))
    .m_out <- unname(.m_out[!is.na(.m_out)])
    m <- m[!m %in% .m_out]
  }

  ## assign points
  predict <- get_assignment(dsc, points, type=assign, method=assignmentMethod, ...)
  #print(table(actual,predict))
  # if we have an outlier detecting clusterer, assignment must have returned both predicted
  # classes and outlier flags
  predict_outliers <- attr(predict,"outliers")
  predict_outliers_corrid <- attr(predict,"outliers_corrid")

  ## translate micro to macro cluster ids if necessary
  if(type=="macro" && assign=="micro") predict <- microToMacro(dsc, predict)
  else if (type!=assign) stop("type and assign are not compatible!")
  #print(table(predict,actual))

  ## predicted noise is still its own class?
  predict[is.na(predict)] <- 0L

  centers <- get_centers(dsc, type=type)

  e <- c()
  for(x in callbacks) {
    m_tmp <- x$env$all_measures[pmatch(tolower(m), tolower(x$env$all_measures))]
    m_tmp <- m_tmp[!is.na(m_tmp)]
    ec <- evaluate_callback(x, dsc, m_tmp, points, actual, predict, outliers, predict_outliers,
                            predict_outliers_corrid, centers, noise)
    e <- c(e, ec)
  }

  structure(e, type=type, assign=assign, class="stream_eval")
}

print.stream_eval <-  function(x, ...) {
  cat("Evaluation results for ", attr(x, "type"),"-clusters.\n", sep="")
  cat("Points were assigned to ", attr(x, "assign"),"-clusters.\n\n", sep="")
  names <- names(x)
  x <- as.numeric(x)
  names(x) <- names
  print(x)
}

# backwards compatibility
evaluate_cluster <- function(dsc, dsd, measure, n=1000, type=c("auto", "micro", "macro"),
                             assign="micro", assignmentMethod =  c("auto", "model", "nn"),
                             horizon=100, verbose=FALSE, noise = c("class", "exclude"), ...) {
  .evaluate_cluster_with_callbacks(dsc, dsd, measure, list(default=DefaultEvalCallback()), n,
                                   type, assign, assignmentMethod, horizon, verbose, noise, ...)
}

evaluate_cluster_with_callbacks <- function(dsc, dsd, measure, callbacks=NULL, n=1000,
                                            type=c("auto", "micro", "macro"),
                                            assign="micro", assignmentMethod =  c("auto", "model", "nn"),
                                            horizon=100, verbose=FALSE,
                                            noise = c("class", "exclude"), ...) {
  if(is.null(callbacks)) callbacks <- list()
  callbacks <- .addMissingDefaultCallback(callbacks)
  .evaluate_cluster_with_callbacks(dsc, dsd, measure, callbacks, n, type, assign, assignmentMethod,
                                   horizon, verbose, noise, ...)
}

## evaluate during clustering
## uses single-fold prequential error estimate (eval and then learn the data)
.evaluate_cluster_with_callbacks <- function(dsc, dsd, measure, callbacks,
                                             n=1000, type=c("auto", "micro", "macro"), assign="micro",
                                             assignmentMethod =  c("auto", "model", "nn"),
                                             horizon=100, verbose=FALSE, noise = c("class", "exclude"),...) {

  if(is.null(callbacks) || !is.list(callbacks) || length(callbacks)<1)
    stop("Callbacks must comprise a list of objects")
  for(x in callbacks)
    if(!is.object(x) || !is(x, "EvalCallback"))
      stop("All callbacks must be derived from EvalCallback")

  rounds <- n %/% horizon
  measure <- c(sapply(callbacks, function(cb_obj) cb_obj$env$all_measures[pmatch(tolower(measure), tolower(cb_obj$env$all_measures))]))
  measure <- measure[!is.na(measure)]

  evaluation <- data.frame(points=seq(from=1, by=horizon, length.out=rounds))
  for(m in measure) evaluation[[m]] <- NA_real_

  for(i in 1:rounds) {
    d <- DSD_Memory(dsd, n=horizon, loop=FALSE)

    ## evaluate first
    reset_stream(d)

    r <- evaluate_with_callbacks(dsc, d, measure, callbacks, horizon, type, assign, assignmentMethod,
                                 noise = noise, ...)
    evaluation[i,] <- c(i*horizon, r)


    if(!is(dsc, "DSC_SinglePass")) {
      ## update model
      reset_stream(d)
      update(dsc, d, horizon)
    }

    if(verbose) print(evaluation[i,])
  }

  evaluation
}

# work horse
.evaluate <- function(measure, predict, actual, points, centers, outliers, predict_outliers,
                      predict_outliers_corrid, dsc, callback_obj) {

  if(is.null(actual) && ! measure %in% .eval_measures_int)
    stop("Evaluation measure not available for streams without cluster labels!")

  res <- switch(measure,
                numMicroClusters	= if(is(try(n <- nclusters(dsc, type="micro"),
                                             silent=TRUE), "try-error")) NA_integer_ else n,
                numMacroClusters	= if(is(try(n <- nclusters(dsc, type="macro"),
                                             silent=TRUE), "try-error")) NA_integer_ else n,
                numClasses	      = numClasses(actual),

                noisePredicted	= sum(predict == 0L),
                noiseActual	    = sum(actual == 0L),
                noisePrecision	= sum(predict == 0L & actual == 0L)/sum(predict == 0L),

                SSQ  	     = ssq(points, actual, predict, centers),
                silhouette = silhouette(points, actual, predict),

                precision	 = precision(actual, predict),
                recall	   = recall(actual, predict),
                F1		     = f1(actual, predict),

                Euclidean	 = clue_agreement(predict, actual, "euclidean"),
                Manhattan	 = clue_agreement(predict, actual, "manhattan"),
                Rand	     = clue_agreement(predict, actual, "rand"),
                cRand	     = clue_agreement(predict, actual, "crand"),
                NMI		     = clue_agreement(predict, actual, "NMI"),
                KP		     = clue_agreement(predict, actual, "KP"),
                angle	     = clue_agreement(predict, actual, "angle"),
                diag	     = clue_agreement(predict, actual, "diag"),
                FM		     = clue_agreement(predict, actual, "FM"),
                Jaccard	   = clue_agreement(predict, actual, "jaccard"),
                #purity	    = clue_agreement(predict, actual, "purity"),
                PS		     = clue_agreement(predict, actual, "PS"),

                purity     = purity(predict, actual),
                #classPurity	    = classPurity(actual, predict),
                OutlierJaccard = outlierJaccard(predict, actual, outliers, predict_outliers, predict_outliers_corrid,
                                                dsc, callback_obj),
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

  FN <- sum(sapply(1:(nrow(conf)-1L),
                   FUN=function(i) conf[i,] * colSums(conf[-(1:i),,drop=FALSE])))

  #TN <- N - FN

  TP/(TP+FN)
}

# precision TP/(TP+FP)
precision <- function(actual, predict) {
  ## TP+FP
  P <- sum(choose(table(predict), 2))
  ## TP
  TP <- sum(choose(table(predict,actual),2))

  TP/P
}

f1 <- function(actual, predict) {
  precision <- precision(actual, predict)
  recall <- recall(actual, predict)
  (2*precision*recall)/(precision+recall)
}

purity <- function(actual, predict) {
  conf <- table(predict, actual)
  mean(colMax(conf)/colSums(conf))
}


## helper
colMax <- function(x, which=FALSE) {
  if(!which) apply(x, 2, FUN=function(y) max(y))
  else {
    apply(x, 2, FUN=function(y) which.max(y))
  }
}

rowMax <- function(x, which=FALSE) {
  if(!which) apply(x, 1, FUN=function(y) max(y))
  else {
    apply(x, 1, FUN=function(y) which.max(y))
  }
}

## FIXME: check!
# as defined in Density-Based Clustering of Data Streams at
# Multiple Resolutions by Wan et al
classPurity <- function(actual, predict) {
  confusion <- table(actual, predict)
  mean(rowMax(confusion)/rowSums(confusion))
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
  if(!is.null(actual)) points <- points[actual != 0L,]

  assign_dist <- apply(dist(points, centers), 1, min)
  sum(assign_dist^2)
}

silhouette <- function(points, actual, predict) {
  ## silhouette does not use noise points
  if(!is.null(actual)) noise <- actual==0 & predict==0
  else noise <- predict==0

  points <- points[!noise,]
  predict <- predict[!noise]

  #  if(any(predict==0)) warning("silhouette: ", sum(predict==0), " non-noise points were predicted noise incorrectly and form their own cluster.")

  ## points that are predicted as noise but are not are its own group!


  mean(cluster::silhouette(predict, dist(points))[,"sil_width"])
}

clue_agreement <- function(predict, actual, measure) {
  predict <- clue::as.cl_hard_partition(predict)
  actual <- clue::as.cl_hard_partition(actual)
  as.numeric(clue::cl_agreement(clue::cl_ensemble(predict, actual), method=measure))
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

outlierJaccard <- function(predict, actual, outliers, predict_outliers, predict_outliers_corrid,
                           dsc, callback_obj) {
  if(missing(dsc)) warning("dsc is missing")
  if(is.null(callback_obj$env$copi))
    cumulative <- list(go=0, tp=0, fp=0, undet=0, tp_corrid=c(), fp_corrid=c(), undet_corrid=c())
  else
    cumulative <- callback_obj$env$copi
  cumulative$go <- cumulative$go + sum(outliers) # the number of generated outliers
  if(missing(predict_outliers) || is.null(predict_outliers) || length(predict_outliers)!=length(outliers)) {
    # we estimate whether the actual class resides in a predicted class that has only one appearance
    # this is for clusterers that have no explicit outlier recognition
    conf <- table(predict, actual) # concidence matrix
    undet_outliers <- tmp_outliers <- actual[which(outliers==TRUE)]
    for(rindex in 1:nrow(conf)) {
      if(sum(conf[rindex,])==1) {
        # This is a detected outlier
        for(cindex in 1:ncol(conf)) {
          # pass over generated classes and check which one has this outlier
          if(conf[rindex,cindex]==1 && sum(conf[,cindex]==1)) {
            # this is obviously one data instance in its own actual and predicted class
            actual_out <- colnames(conf)[[cindex]]
            if(actual_out %in% tmp_outliers) {
              undet_outliers <- undet_outliers[undet_outliers!=actual_out]
              cumulative$tp <- cumulative$tp + 1 # it was marked by the stream, therefore true positive
            } else cumulative$fp <- cumulative$fp + 1 # it was NOT marked by the stream, therefore false positive
          }
        }
      }
    }
    cumulative$undet <- cumulative$undet + length(undet_outliers)
    if(cumulative$go>0) cumulative$oji <- cumulative$tp/(cumulative$tp+cumulative$fp+cumulative$undet)
    else cumulative$oji <- 0.0
  } else {
    # this is matching for clusterers that have explicit outlier recognition
    act_out <- which(outliers)
    pred_out <- which(predict_outliers)
    cumulative$undet_corrid <- unique(c(cumulative$undet_corrid, predict_outliers_corrid[act_out[which(!act_out %in% pred_out)]]))
    cumulative$tp_corrid <- unique(c(cumulative$tp_corrid, predict_outliers_corrid[act_out[which(act_out %in% pred_out)]]))
    cumulative$fp_corrid <- unique(c(cumulative$fp_corrid, predict_outliers_corrid[pred_out[which(!pred_out %in% act_out)]]))
    if(dsc$recheck_outliers) {
      for(o_id in cumulative$tp_corrid)#[which(!cumulative$tp_corrid %in% p2)]) {
        if(!recheck_outlier(dsc, o_id)) {
          cumulative$tp_corrid <- cumulative$tp_corrid[cumulative$tp_corrid!=o_id] # remove it from the TP
          cumulative$undet_corrid <- c(cumulative$undet_corrid, o_id) # but we add it to the undetected set
        }
      for(o_id in cumulative$fp_corrid)#[which(!cumulative$fp_corrid %in% p2)])
        if(!recheck_outlier(dsc, o_id))
          cumulative$fp_corrid <- cumulative$fp_corrid[cumulative$fp_corrid!=o_id]
    }
    #message(paste("OJI TP=",length(cumulative$tp_corrid),"FP=",length(cumulative$fp_corrid),"UNDET=",length(cumulative$undet_corrid)))
    cumulative$oji <- length(cumulative$tp_corrid)/(length(cumulative$tp_corrid)+length(cumulative$fp_corrid)+length(cumulative$undet_corrid))
  }
  callback_obj$env$copi <- cumulative
  cumulative$oji
}

.addMissingDefaultCallback <- function(callbacks) { # we check and add default callback if mising
  default <- F
  for(x in callbacks) {
    if(is(x, "DefaultEvalCallback")) default <- T
  }
  if(!default) callbacks$default <- DefaultEvalCallback()
  callbacks
}
