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
  
.all_measures <- c(.eval_measures_int, .eval_measures_ext, 
    .eval_measures_fpc_int, .eval_measures_fpc_ext)

evaluate <- function (dsc, dsd, measure, n = 100, 
  type=c("auto", "micro", "macro"), 
  assign="micro", 
  assignmentMethod=c("auto","model", "nn"),
  noise = c("class", "exclude"),
  ...) {
  
  assignmentMethod <- match.arg(assignmentMethod)
  noise <- match.arg(noise)
  type <- get_type(dsc, type)  
  
  points <- get_points(dsd, n, cluster = TRUE)
  actual <- attr(points, "cluster")
  
  if(missing(measure) || is.null(measure)) {
    if(!is.null(actual)) m <- .all_measures
    else m <- c(.eval_measures_int, .eval_measures_fpc_int)
  } else m <- .all_measures[pmatch(tolower(measure), tolower(.all_measures))] 
  
  if(any(is.na(m))) 
    stop("Invalid measure: ", paste(measure[is.na(m)], collapse=', '))
  
  
  if(is.null(actual) && ! measure %in% c(.eval_measures_int, 
    .eval_measures_fpc_int)) 
    stop("External evaluation measures not available for streams without cluster labels!")
  
  centers <- get_centers(dsc, type=type) 
  
  ## no centers available
  if(nrow(centers)<1) {
    #warning("No centers available!")
    e <- rep.int(NA_real_, length(measure))
    e[m %in% c("numMicroClusters", "numMacroClusters")] <- 0
    names(e) <- m
    return(structure(e, type=type, assign=assign, class="stream_eval"))
  }
  
  ## assign points
  predict <- get_assignment(dsc, points, type=assign, method=assignmentMethod, ...)
  
  ## translate micro to macro cluster ids if necessary
  if(type=="macro" && assign=="micro") predict <- microToMacro(dsc, predict)
  else if (type!=assign) stop("type and assign are not compatible!")
  
  ## predicted noise is still its own class?
  predict[is.na(predict)] <- 0L
  
  fpc <- m %in% c(.eval_measures_fpc_int, .eval_measures_fpc_ext)
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
    
    }else if(noise=="exclude") {
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
      silhouette = TRUE,
      G2 = TRUE, G3 = FALSE,
      wgap=FALSE, sepindex=FALSE, sepprob=0.1,
      sepwithnoise=withnoise,
      compareonly = FALSE,
      aggregateonly = TRUE)
    e <- unlist(e)
  }else e <- numeric()
  
  if(any(!fpc)) {
    
    ## deal with noise
    if(noise=="class") {
      ## noise it its own group with index 0: this works for external measures
      if(!is.null(actual)) actual[is.na(actual)] <- 0L
      predict[is.na(predict)] <- 0L
    }else if(noise=="exclude") {
      ## remove all actual noise points
      if(!is.null(actual)) {
        nsp <- is.na(actual)
        actual <- actual[!nsp]
        predict <- predict[!nsp]
        points <- points[!nsp, , drop = FALSE]
      }
    } else stop("Unknown noise treatment!")
    
    v <- sapply(m[!fpc], function(x) .evaluate(x, predict, actual, 
      points, centers, dsc))
    e <- c(e, v)
  }
  
  e <- e[m]
  
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

## evaluate during clustering 
## uses single-fold prequential error estimate (eval and then learn the data)
evaluate_cluster <- function(dsc, dsd, measure, 
  n=1000, type=c("auto", "micro", "macro"), assign="micro",
  assignmentMethod =  c("auto", "model", "nn"),
  horizon=100, verbose=FALSE, noise = c("class", "exclude"), ...) {
  
  rounds <- n %/% horizon 
  measure <- .all_measures[pmatch(tolower(measure), tolower(.all_measures))] 
  
  evaluation <- data.frame(points=seq(from=1, by=horizon, length.out=rounds)) 
  for(m in measure) evaluation[[m]] <- NA_real_  
  
  for(i in 1:rounds) {
    d <- DSD_Memory(dsd, n=horizon, loop=FALSE)
    
    ## evaluate first
    reset_stream(d)
    evaluation[i,] <- c((i-1)*horizon, evaluate(dsc, d, measure, 
      horizon, type, assign, assignmentMethod, noise = noise, ...))
    
    ## update model
    reset_stream(d)
    update(dsc, d, horizon)
    
    if(verbose) {
      print(evaluation[i,])
    }
  }
  
  evaluation
}

# work horse
.evaluate <- function(measure, predict, actual, points, centers, dsc) {
  
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
