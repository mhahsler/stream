SHCAgglomerationType <- list(NormalAgglomeration=0,AggresiveAgglomeration=1,RelaxedAgglomeration=2)
SHCDriftType <- list(NormalDrift=0,FastDrift=1,SlowDrift=2,NoDrift=3,UltraFastDrift=4)
.reserved <- c("class","assigned","clazz","component","cids","isOutlier","stopHere","clazzAsOutlier")

stream.SHC <- setRefClass("stream.SHC",
                   fields = list(
                     shc="ANY",
                     recStats="logical",
                     stat_val="data.frame",
                     stat_idx="numeric"
                   ),
                   methods = list(
                     initialize=function(dimensions,aggloType=SHCAgglomerationType$NormalAgglomeration,driftType=SHCDriftType$NormalDrift,
                                decaySpeed=10,sharedAgglomerationThreshold=1,recStats=FALSE,sigmaIndex=FALSE,sigmaIndexNeighborhood=3,
                                sigmaIndexPrecisionSwitch=TRUE) {
                       recStats <<- recStats
                       stat_val <<- data.frame()
                       stat_idx <<- 1
                       shc <<- new(SHC_R,dimensions,aggloType,driftType,decaySpeed,sharedAgglomerationThreshold)
                       if(sigmaIndex)
                         shc$useSigmaIndex(sigmaIndexNeighborhood,sigmaIndexPrecisionSwitch)
                     }
))

stream.SHC$methods(list(
  copy = function(...) {
    n <- stream.SHC(shc)
    return(n)
  },

  cache = function(...){
  },

  uncache = function(...) {
  },

  process = function(newdata) {
    if(!is.data.frame(newdata))
      stop("Submitted data parameter must be a data frame")
    return(shc$process(newdata,F))
  },

  cluster=function(newdata,type=c("none", "auto", "micro", "macro"),...) {
    if(!is.data.frame(newdata))
      stop("Submitted data parameter must be a data frame")
    type <- match.arg(type)
    ret <- shc$process(newdata,F)
    if(recStats) {
      s <- shc$stats()
      stat_val <<- rbind(stat_val,data.frame(index=stat_idx,components=s$components,outliers=s$outliers))
      stat_idx <<- stat_idx + 1
    }
    ret <- cbind(ret,data.frame(outlier_id=ret[,"component_id"]))
    ret[!ret$outlier,"outlier_id"] <- NA
    #ret[ret$outlier,"assigned_comp"] <- NA
    #ret[ret$outlier,"assigned_cluster"] <- NA
    if(type=="none" || type=="auto" || type=="micro")
      predict <- ret[,"assigned_comp"]
    else
      predict <- ret[,"assigned_cluster"]
    attr(predict,"outliers") <- ret[,"outlier"]
    attr(predict,"outliers_corrid") <- ret[,"outlier_id"]
    predict
  },

  getStats=function() {
    return(stat_val)
  },

  getComponentAndOutlierStatistics=function() {
    return(shc$stats())
  },

  get_assignment=function(newdata,type=c("none", "auto", "micro", "macro"),...) {
    if(!is.data.frame(newdata))
      stop("Submitted data parameter must be a data frame")
    type <- match.arg(type)
    ret <- shc$process(newdata,T)
    ret <- cbind(ret,data.frame(outlier_id=ret[,"component_id"]))
    ret[!ret$outlier,"outlier_id"] <- NA
    #ret[ret$outlier,"assigned_comp"] <- NA
    #ret[ret$outlier,"assigned_cluster"] <- NA
    if(type=="none" || type=="auto" || type=="micro")
      predict <- ret[,"assigned_comp"]
    else
      predict <- ret[,"assigned_cluster"]
    attr(predict,"outliers") <- ret[,"outlier"]
    attr(predict,"outliers_corrid") <- ret[,"outlier_id"]
    predict
  },

  get_microclusters=function(...) {
    df <- data.frame()
    for(compId in shc$getAllComponents()) {
      compDesc <- shc$getComponentDetails(compId)
      df <- rbind(df, data.frame(matrix(compDesc$mean, ncol=length(compDesc@mean))))
    }
    return(df)
  },

  get_microweights=function(...) {
    df <- data.frame()
    for(compId in shc$getAllComponents()) {
      df <- rbind(df, data.frame(W=shc$getComponentWeight(compId)))
    }
    return(df)
  },

  get_macroclusters=function(...) {
    df <- data.frame()
    for(clusId in shc$getClusters(T,T)) {
      tw <- 0
      cen <- NULL
      dim <- 0
      for(compId in shc$getComponents(clusId)) {
        compDesc <- shc$getComponentDetails(compId)
        if(is.null(cen)) cen <- rep(0,compDesc$dimensions)
        dim <- length(compDesc$mean)
        tw <- tw+compDesc$elements;
        cen <- cen+(compDesc$elements*compDesc$mean)
      }
      df <- rbind(df, data.frame(matrix(cen/tw, ncol=dim)))
    }
    return(df)
  },

  get_macroweights=function(...) {
    df <- data.frame()
    for(clusId in shc$getClusters(T,T)) {
      df <- rbind(df, data.frame(W=shc$getClusterWeigth(clusId)))
    }
    return(df)
  },

  microToMacro=function(micro, ...) {
    if(is.null(micro) || all(is.na(micro)) || length(micro)==0)
      return(c(0))
    return(shc$microToMacro(micro))
  },

  microToMicro=function(micro, ...) { # this mapps obsolete, removed and agglomerated components to new ones
    if(is.null(micro) || all(is.na(micro)) || length(micro)==0)
      return(c(0))
    res <- c()
    for(i in 1:length(micro)) {
      assigned_compId <- micro[[i]] # this is not SHC component id, it is a mapped value needed for the stream package
      for(nassigned_compId in shc$microToMicro(assigned_compId))
        if(nassigned_compId>0) res <- append(res, nassigned_compId)
    }
    if(length(res)==0) return(c(0)) # return UNASSIGNED if no new components has been found
    return(res)
  },

  setPseudoOfflineCounter=function(counter, ...) {
    shc$setPseudoOfflineCounter(counter)
  },

  getTimes=function(...) {
    return(shc$getTimes())
  },

  getNodeCounter=function(...) {
    return(shc$getNodeCounter())
  },

  getComputationCostReduction=function(...) {
    return(shc$getComputationCostReduction())
  },

  getHistogram=function(...) {
    return(shc$getHistogram())
  },

  recheckOutlier=function(id,...) {
    return(shc$recheckOutlier(id))
  },

  getOutlierPositions=function(...) {
    return(shc$getOutlierPositions())
  },

  getTrace=function(id,...) {
    return(shc$getTrace(id))
  },

  clearEigenMPSupport=function() {
    shc$clearEigenMPSupport();
  }
))

stream.SHC.clone <- setRefClass("stream.SHC.clone",
                                contains = "stream.SHC",
                                methods = list(
                                  initialize=function(old_shc) {
                                    stat_val <<- data.frame()
                                    stat_idx <<- 1
                                    recStats <<- FALSE
                                    shc <<- old_shc$shc$cloneSHC()
                                  }
                          ))

stream.SHC.man <- setRefClass("stream.SHC.man",
                                contains = "stream.SHC",
                                methods = list(
                                  initialize=function(dimensions,theta,virtualVariance,parallelize=FALSE,performSharedAgglomeration=TRUE,
                                                      agglo_count=100,cbVarianceLimit=as.double(10.0),cbNLimit=as.integer(40),
                                                      driftRemoveCompSizeRatio=as.double(0.3),driftCheckingSizeRatio=as.double(1.3),
                                                      driftMovementMDThetaRatio=as.double(0.8),decaySpeed=as.integer(10),
                                                      sharedAgglomerationThreshold=as.integer(1),compFormingMinVVRatio=as.double(0.2),
                                                      compBlockingLimitVVRatio=as.double(0.0),recStats=FALSE,sigmaIndex=FALSE,
                                                      sigmaIndexNeighborhood=3,sigmaIndexPrecisionSwitch=TRUE) {
                                    recStats <<- recStats
                                    stat_val <<- data.frame()
                                    stat_idx <<- 1
                                    vv <- rep(virtualVariance,dimensions)
                                    params <- list(theta=theta,parallelize=parallelize,performSharedAgglomeration=performSharedAgglomeration,
                                                   virtualVariance=vv,agglo_count=agglo_count,cbVarianceLimit=cbVarianceLimit,
                                                   cbNLimit=cbNLimit,driftRemoveCompSizeRatio=driftRemoveCompSizeRatio,
                                                   driftCheckingSizeRatio=driftCheckingSizeRatio,driftMovementMDThetaRatio=driftMovementMDThetaRatio,
                                                   decayPace=decaySpeed,sharedAgglomerationThreshold=sharedAgglomerationThreshold,
                                                   componentFormingMinVVRatio=compFormingMinVVRatio,
                                                   componentBlockingLimitVVRatio=compBlockingLimitVVRatio)
                                    shc <<- new(SHC_R,params)
                                    if(sigmaIndex)
                                      shc$useSigmaIndex(sigmaIndexNeighborhood,sigmaIndexPrecisionSwitch)
                                  }
                                ))

DSC_SHC.behavioral <- function(dimensions,aggloType=SHCAgglomerationType$NormalAgglomeration,
                               driftType=SHCDriftType$NormalDrift,decaySpeed=10,sharedAgglomerationThreshold=1,
                               recStats=FALSE,sigmaIndex=FALSE,sigmaIndexNeighborhood=3,sigmaIndexPrecisionSwitch=TRUE) {
  x <- stream.SHC(dimensions,aggloType,driftType,decaySpeed,sharedAgglomerationThreshold,recStats,
                  sigmaIndex,sigmaIndexNeighborhood,sigmaIndexPrecisionSwitch)
  macro <- new.env()
  macro$theta <- x$shc$theta()
  macro$virtualVariance <- x$shc$virtualVariance()

  structure(
    list(
      description = "Statistical Hierarchical Clustering",
      RObj = x,
      recheck_outliers = T,
      macro = macro
    ), class = c("DSC_SHC", "DSC_SinglePass", "DSC_Outlier", "DSC_Micro", "DSC_R", "DSC")
  )
}

DSC_SHC.man <- function(dimensions,theta,virtualVariance,parallelize=FALSE,performSharedAgglomeration=TRUE,
                    compAssimilationCheckCounter=50,cbVarianceLimit=as.double(10.0),cbNLimit=as.integer(40),
                    driftRemoveCompSizeRatio=as.double(0.3),driftCheckingSizeRatio=as.double(1.3),
                    driftMovementMDThetaRatio=as.double(0.8),decaySpeed=as.integer(10),
                    sharedAgglomerationThreshold=as.integer(1),compFormingMinVVRatio=as.double(0.2),
                    compBlockingLimitVVRatio=as.double(0.0),recStats=FALSE,sigmaIndex=FALSE,
                    sigmaIndexNeighborhood=3,sigmaIndexPrecisionSwitch=TRUE) {
  x <- stream.SHC.man(dimensions,theta,virtualVariance,parallelize,performSharedAgglomeration,
                      compAssimilationCheckCounter,cbVarianceLimit,cbNLimit,driftRemoveCompSizeRatio,driftCheckingSizeRatio,
                      driftMovementMDThetaRatio,decaySpeed,sharedAgglomerationThreshold,
                      compFormingMinVVRatio,compBlockingLimitVVRatio,recStats,sigmaIndex,
                      sigmaIndexNeighborhood,sigmaIndexPrecisionSwitch)
  macro <- new.env()
  macro$theta <- x$shc$theta()
  macro$virtualVariance <- x$shc$virtualVariance()

  structure(
    list(
      description = "Statistical Hierarchical Clustering",
      RObj = x,
      recheck_outliers = T,
      macro = macro
    ), class = c("DSC_SHC", "DSC_SinglePass", "DSC_Outlier", "DSC_Micro", "DSC_R", "DSC")
  )
}

get_stats <- function(x) UseMethod("get_stats")
get_stats.default <- function(x, ...) {
  stop(gettextf("get_stats not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
get_stats.DSC_SHC <- function(x, ...) {
  return(x$RObj$getStats())
}

get_microclusters.DSC_SHC <- function(x,...){
  return(x$RObj$get_microclusters(...))
}

get_microweights.DSC_SHC <- function(x, ...) {
  return(x$RObj$get_microweights(...))
}

get_macroclusters.DSC_SHC <- function(x,...){
  return(x$RObj$get_macroclusters(...))
}

get_macroweights.DSC_SHC <- function(x, ...) {
  return(x$RObj$get_macroweights(...))
}

microToMacro.DSC_SHC <- function(x, micro=NULL, ...) {
  return(x$RObj$microToMacro(micro,...))
}

get_assignment.DSC_SHC <- function(dsc, points, type=c("auto", "micro", "macro"), method=c("auto", "model", "nn"), ...) {
  return(dsc$RObj$cluster(points,type,...))
}

get_outlier_positions.DSC_SHC <- function(x, ...) {
  return(x$RObj$getOutlierPositions())
}

recheck_outlier.DSC_SHC <- function(x, outlier_correlated_id, ...) {
  return(x$RObj$recheckOutlier(outlier_correlated_id))
}

plot.DSC_SHC <- function(x, dsd = NULL, n = 500, type = c("auto", "micro", "macro", "both"),
                         displayDataPoints=TRUE, ...) {
  if(!requireNamespace("ggplot2"))
    message("Plotting for the Statistical Hierarchical Clusterer requires ggplot2 package")
  else {
    #loadNamespace("ggplot2")
    shc_plot <- ggplot2::ggplot() + ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(), legend.position = "none")
    if(displayDataPoints) {
      d <- get_points(dsd, n, cluster = TRUE, outlier = TRUE)
      assignment_c <- attr(d, "cluster")
      assignment_o <- attr(d, "outlier")
      d_clus <- d[which(!assignment_o),]
      d_clus_col <- assignment_c[which(!assignment_o)]
      colnames(d_clus) <- paste0("X", 1:ncol(d_clus))
      d_out <- d[which(assignment_o),]
      d_out_col <- assignment_c[which(assignment_o)]
      colnames(d_out) <- paste0("X", 1:ncol(d_out))
      shc_plot <- shc_plot +
        ggplot2::geom_point(data = d_clus, ggplot2::aes_string(x="X1", y="X2", colour=factor(d_clus_col)), size=1, show.legend=F) +
        ggplot2::geom_point(data = d_out, ggplot2::aes_string(x="X1", y="X2", colour=factor(d_out_col)), shape=8, size=2, show.legend=F)
    }

    clusts <- x$RObj$shc$getClusters(T,F)
    #print(paste0("Clusters [",paste(clusts,collapse=","),"]"))
    outls <- x$RObj$shc$getClusters(F,T)
    #print(paste0("Outliers [",paste(outls,collapse=","),"]"))
    for(clus_id in c(clusts)) {
      comps <- x$RObj$shc$getComponents(clus_id)
      cd <- x$RObj$shc$getClusterContours(clus_id)
      for(comp_id in comps) {
        details <- as.data.frame(cd[comp_id]);
        colnames(details)=c("X1","X2")
        shc_plot <- shc_plot + ggplot2::geom_path(data=details, ggplot2::aes_string(x="X1", y="X2"), show.legend=FALSE, size=0.9, colour="red")
      }
    }
    for(clus_id in c(outls)) {
      comps <- x$RObj$shc$getComponents(clus_id)
      cd <- x$RObj$shc$getClusterContours(clus_id)
      for(comp_id in comps) {
        details <- as.data.frame(cd[comp_id]);
        colnames(details)=c("X1","X2")
        shc_plot <- shc_plot + ggplot2::geom_path(data=details, ggplot2::aes_string(x="X1", y="X2"), show.legend=FALSE, size=0.7, colour="green", linetype="dotted")
      }
    }
    plot(shc_plot)
  }
}

get_times <- function(x) UseMethod("get_times")
get_times.default <- function(x,...) {
  stop(gettextf("get_times not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
get_times.DSC_SHC <- function(x,...) {
  return(x$RObj$getTimes())
}

get_node_counter <- function(x) UseMethod("get_node_counter")
get_node_counter.default <- function(x,...) {
  stop(gettextf("get_node_counter not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
get_node_counter.DSC_SHC <- function(x,...) {
  return(x$RObj$getNodeCounter())
}

get_computation_cost_reduction <- function(x) UseMethod("get_computation_cost_reduction")
get_computation_cost_reduction.default <- function(x,...) {
  stop(gettextf("get_computation_cost_reduction not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
get_computation_cost_reduction.DSC_SHC <- function(x,...) {
  return(x$RObj$getComputationCostReduction())
}

clean_outliers.DSC_SHC <- function(x, ...) {
  x$RObj$clean_outliers()
}

setPseudoOfflineCounter <- function(x,counter) UseMethod("setPseudoOfflineCounter")
setPseudoOfflineCounter.default <- function(x,counter, ...) {
  stop(gettextf("setPseudoOfflineCounter not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
setPseudoOfflineCounter.DSC_SHC <- function(x,counter, ...) {
  x$RObj$setPseudoOfflineCounter(counter)
}

getHistogram <- function(x) UseMethod("getHistogram")
getHistogram.default <- function(x, ...) {
  stop(gettextf("getHistogram not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
getHistogram.DSC_SHC <- function(x, ...) {
  x$RObj$getHistogram()
}

clearEigenMPSupport <- function(x) UseMethod("clearEigenMPSupport")
clearEigenMPSupport.default <- function(x, ...) {
  stop(gettextf("clearEigenMPSupport not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}
clearEigenMPSupport.DSC_SHC <- function(x, ...) {
  x$RObj$clearEigenMPSupport()
}

.shc_measures <- c("queryTime","updateTime","processTime","nodeCount","computationCostReduction")

SHCEvalCallback <- function() {
  env = environment()
  all_measures <- .shc_measures
  internal_measures <- .shc_measures
  external_measures <- c()
  outlier_measures <- c()
  this <- list(
    description = "SHC evaluation callback",
    env = env
  )
  class(this) <- c("SHCEvalCallback", "EvalCallback")
  this
}
evaluate_callback.SHCEvalCallback <- function(cb_obj, dsc, measure, points, actual, predict, outliers,
                                              predict_outliers, predict_outliers_corrid,
                                              centers, noise, ...) {
  r <- list()
  times <- get_times(dsc)
  if("queryTime" %in% measure) r$queryTime <- times$queryTime
  if("updateTime" %in% measure) r$updateTime <- times$updateTime
  if("processTime" %in% measure) r$processTime <- times$processTime
  if("nodeCount" %in% measure) r$nodeCount <- get_node_counter(dsc)
  if("computationCostReduction" %in% measure) r$computationCostReduction <- get_computation_cost_reduction(dsc)*100
  r
}

DSC_registry$set_entry(name = "DSC_SHC",
                       DSC_Micro = TRUE, DSC_Macro = TRUE, DSC_Outlier = TRUE, DSC_SinglePass = TRUE,
                       description = "SHC - Statistical Hierarchical Clusterer")
