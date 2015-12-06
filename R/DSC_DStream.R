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

DSC_DStream <- function(gridsize, lambda = 1e-3, 
  gaptime=1000L, Cm=3, Cl=.8, attraction=FALSE, epsilon=.3, 
  Cm2=Cm, k=NULL, N=0) {
  
  dstream <- dstream$new(gridsize, lambda, 
    gaptime, Cm, Cl, attraction, epsilon, 
    Cm2, k, N)
  
  macro <- new.env()
  macro$newdata <- TRUE
  
  structure(
    list(
      description = "DStream",
      RObj = dstream,
      macro = macro
    ), class = c("DSC_DStream", "DSC_Micro", "DSC_R", "DSC")
  )
}

dstream <- setRefClass("dstream",
  fields = list(
    gridsize		    = "numeric",
    ### decay (note lambda is different than in the paper!)
    lambda			    = "numeric",
    gaptime         = "integer",
    ### dense grid threshold Cm > 1 -> Dm = Cm/(N*(1-decay_factor))
    Cm              = "numeric",
    ### sparse grid threshold 0<Cl<1 -> Dl = Cl/(N*(1-decay_factor))
    Cl              = "numeric",
    ### other grid types
    ### transitional grid Cl < d < Cm
    ### sporadic grid pi = (Cl * (1-decay_factor))/(N*(1-decay_factor))
    ### for large t -> 1/decay_factor
    
    ### attraction boundary (FIXME: Needs to be implemented)
    attraction      = "logical",
    epsilon		      = "numeric",
    Cm2		          = "numeric",
    k               = "integer",
    N_fixed         = "logical",
    N               = "numeric",
    
    ### store the grid
    micro 		      = "ANY",
    serial          = "ANY",
    decay_factor		= "numeric"
  ),
  
  methods = list(
    initialize = function(
      gridsize = 0.1,
      lambda   = 1e-3,
      gaptime  = 1000L,
      Cm       = 3,
      Cl       = .8,
      attraction = FALSE,
      epsilon  = .3,
      Cm2      = 3,
      k        = NULL,
      N        = 0
    ) {
      
      gridsize  <<- gridsize
      lambda    <<- lambda
      gaptime	  <<- as.integer(gaptime)
      Cm        <<- Cm
      Cl        <<- Cl
      attraction <<- as.logical(attraction)
      epsilon   <<- epsilon
      Cm2       <<- Cm2
      N         <<- N
      
      if(is.null(k)) k <<- 0L
      else k <<- as.integer(k)
      
      if(N==0) N_fixed <<- FALSE
      else N_fixed <<- TRUE
      
      ### this is what the paper calls lambda!
      decay_factor <<- 2^(-lambda)
      
      micro <<- new(DStream, gridsize, decay_factor, gaptime,
        Cl, N, attraction, epsilon)
      
      .self
    }
  )
)

dstream$methods(list(
  # overload copy
  copy = function(...) {
    #callSuper(...)
    ### copy S4 object
    n <- dstream$new(gridsize, decay_factor, gaptime,
      Cl, N, attraction, epsilon)
    
    ### copy Rcpp object  
    n$micro <- new(DStream, micro$serializeR())
    
    n  
  },
  
  cache = function(){ 
    serial <<- micro$serializeR()
  },
  
  uncache = function() {
    micro <<- new(DStream, serial)
    serial <<- NULL
  },
  
  
  cluster = function(newdata, debug = FALSE) {
    'Cluster new data.' ### online help
    
    micro$update(as.matrix(newdata), debug)
  },
  
  ### This is for plotting images.
  toMatrix = function(grid_type="used", dim=NULL) {
    
    ### nothing clustered yet
    if(!length(micro$mins) || !length(micro$maxs)) return(matrix(0, nrow=0, ncol=0))
    
    cs <- get_micro(weight = TRUE, grid_type = grid_type, translate = FALSE) 
    ws <- attr(cs, "weight")
    
    ns <- (micro$maxs-micro$mins)+1L
    mat <- matrix(0, nrow=ns[1], ncol=ns[2])
    
    if(nrow(cs)>0) {
      ## dimensions to show
      if(!is.null(dim)) cs <- cs[, dim[1:2], drop = FALSE]
      else cs <- cs[, 1:2, drop = FALSE]
      
      for(i in 1:nrow(cs)) mat[cs[i,1]-micro$mins[1]+1, 
        cs[i,2]-micro$mins[2]+1] <- ws[i]
    }
     
    rownames(mat) <- (micro$mins[1]:micro$maxs[1])*gridsize + gridsize/2
    colnames(mat) <- (micro$mins[2]:micro$maxs[2])*gridsize + gridsize/2
    ### FIXME: Colnames!
    attr(mat, "varnames") <- colnames(cs)    
    
    mat
  },
  
  get_attraction = function(relative=FALSE, grid_type="dense", dist=FALSE) {
    if(!attraction) stop("No attraction values stored. ",
      "Create the DSC_DStream with attraction=TRUE.")
    
    if(dist) stop("dist not implemented yet...")
    
    attr_matrix <-  micro$getAttraction()
    
    if(relative) {
      w <- attr(get_micro(weight=TRUE), "weight")
      attr_matrix <- attr_matrix/w
    }
    
    c_type <- mc_type(grid_type)
    used <- attr(c_type, "used")
    attr_matrix <- attr_matrix[used, used, drop=FALSE]
     
    if(dist) as.dist(attr_matrix) ### FIXME: make symetric first 
    else attr_matrix  
  },
  
  get_micro = function(weight=FALSE, cluster_type=FALSE, translate=TRUE, 
    grid_type=c("used", "dense", "transitional", "sparse", "all")) {
    
    grid_type <- match.arg(grid_type)
    
    cs <- micro$centers(FALSE)  # no decoding
    ws <- micro$weights()
    
    if(length(ws) == 0) {
      ret <- data.frame()
      if(weight) attr(ret, "weight") <- numeric(0)
      if(cluster_type) attr(ret, "cluster_type") <- factor(levels = 
          c("dense", "transitional", "sparse"))
      else return(ret)
    }
    
    c_type <- mc_type(grid_type) ## updates N
    used <- attr(c_type, "used")
    cs <- cs[used, , drop=FALSE]
    ws <- ws[used]
    c_type <- c_type[used]
    
    ### translate coordinates?
    if(translate && nrow(cs)>0) cs <- cs*gridsize + gridsize/2
    
    if(weight) attr(cs, "weight") <- ws
    if(cluster_type) attr(cs, "cluster_type") <- c_type 
    rownames(cs) <- NULL
    cs
  },
  
  mc_type = function(grid_type) {
    cs <- micro$centers(FALSE)  # no decoding
    ws <- micro$weights()
    
    ## update N?
    if(!N_fixed) N <<- prod(micro$maxs-micro$mins+1L)
    
    c_type <- factor(rep.int("sparse", times=length(ws)), 
      levels=c("dense", "transitional", "sparse"))
    c_type[ws > Cl/(N*(1-decay_factor))] <- "transitional"
    c_type[ws > Cm/(N*(1-decay_factor))] <- "dense"
    
    if(grid_type=="all") {
      used <- rep(TRUE, time = length(c_type))
    } else if(grid_type!="used") {
      used <- c_type==grid_type
    }else{
      # dense + adjacent transitional
      dense <- c_type=="dense"
      trans <- c_type=="transitional"
      used <- dense
      
      if(any(dense) && any(trans)) {
        take <- dist(cs[trans,,drop=FALSE], cs[dense,,drop=FALSE], method="Manhattan") <= 1
        take <- apply(take, 1L, any)
        used[which(trans)[take]] <- TRUE
      }
    }
    
    attr(c_type, "used") <- used
    c_type
  },
  
  get_microclusters = function(...) {  
    get_micro(...)
  },
  
  get_microweights = function(...){ 
    attr(get_micro(weight=TRUE, ...), "weight")
  },
  
  get_macro_clustering = function(...) {
    
    mcs <- get_micro(grid_type="used", translate=FALSE,
      weight=TRUE, cluster_type=TRUE)
    ws <- attr(mcs, "weight")    
    c_type <- attr(mcs, "cluster_type")    
    
    ### no mcs
    if(nrow(mcs) < 1)
      return(list(centers=data.frame(), weights=numeric(0), microToMacro=integer(0)))
    
    ### single mc
    if(nrow(mcs) == 1)
      return(list(centers=mcs, weights=ws, microToMacro=structure(1L, names="1")))
    
    denseID <- c_type=="dense"
    dense <- mcs[denseID,, drop=FALSE]
    transID <- c_type=="transitional"
    trans <- mcs[transID,, drop=FALSE]
    
    
    if(attraction) { ### use attraction
      a <- get_attraction(grid_type="dense")
      
      if(nrow(a)>1) {
        d_attr <- as.dist(-a-t(a))
        
        if(k > 0L)  { ### use k?
          
          hc <- hclust(d_attr, method="single")
          ### find unconnected components
          assignment <- cutree(hc, h=0-1e-9)
          
          maxk <- min(k, nrow(a))
          ### not enough components?
          if(length(unique(assignment)) < maxk) assignment <- cutree(hc, k=maxk)
          
          ### FIXME: If k>number of connected components then components would
          ###  be merged randomly! So we add for these the regular distance!      
          
          #d_dist <- dist(mcs) 
          #unconnected <- d_attr==0 ### an attraction count of 0!
          #d_attr[unconnected] <- d_attr[unconnected] + d_dist[unconnected]
          #assignment <- cutree(hclust(d_attr, method="single"), k=k)
          
        }else{ ### use Cm2 
          
          P <- 2*sum(micro$maxs-micro$mins) ### number of possible attraction values
          ### actually we should check each direction independently
          assignment <- cutree(hclust(d_attr, method="single"), 
            h=-2*Cm2/P/(1+decay_factor))
        }
      }else assignment <- 1L
      
    }else{ ### use adjacency 
      if(nrow(dense)>1) {
        d_pos <- dist(dense)
        assignment <- cutree(hclust(d_pos, method="single"), 
          h=1.1) ### anything less than 2^.5 is fine
      }else assignment <- 1L
    }
    
    ### assign transitional grids
    if(nrow(trans)>0) {
      ass <- rep.int(NA_integer_, length(c_type))
      ass[denseID] <- assignment
      
      # this assigns it to one of the neighboring macro clusters
      take <- dist(trans, dense, method="Manhattan") <= 1
      take <- apply(take, 1L, FUN=function(x) which(x)[1])
      ass[transID] <- assignment[take]
      assignment <- ass
    }
    
    ### translate mcs
    mcs <- mcs*gridsize + gridsize/2
    
    m2m <-  structure(assignment, names=1:length(assignment))
    
    ### find centroids
    macro <- .centroids(mcs, ws, m2m)
    macro$microToMacro <- m2m 
    
    macro
  }
)
)

get_attraction <- function(x, relative=FALSE, grid_type="dense", dist=FALSE) 
  x$RObj$get_attraction(relative=relative, grid_type=grid_type, dist=dist)

get_macroclusters.DSC_DStream <- function(x,...){
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering(...)
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$centers
}

get_macroweights.DSC_DStream <- function(x, ...) {
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering(...)
    x$macro$newdata <- FALSE
  }
  
  x$macro$macro$weights
}

microToMacro.DSC_DStream <- function(x, micro=NULL, ...) {
  .nodots(...)
  if(x$macro$newdata) {
    x$macro$macro <- x$RObj$get_macro_clustering()
    x$macro$newdata <- FALSE
  }
  
  assignment <- x$macro$macro$microToMacro
  if(!is.null(micro)) assignment <- assignment[micro]
  assignment
}

### add plot as a grid
plot.DSC_DStream <- function(x, dsd=NULL, n=500, 
  type=c("micro", "macro", "both"), 
  grid=FALSE, grid_type="used", assignment=FALSE,
  ...) {
  
  ### find type
  dim <- list(...)$dim
  main <- list(...)$main
  
  ### grid uses a darker color for the points
  col_points <- list(...)$col_points
  if(is.null(col_points)) col_points <- gray(.1, alpha=.3)
  
  type <- match.arg(type)
  
  ### assignment == grid
  if(assignment) grid <- TRUE
  
  ### implements grid and grid_both
  if(!grid) return(plot.DSC(x, dsd=dsd, n=n, type=type, ...))
  
  #if(nclusters) {
  #  warning("No clusters to plot!")
  #  return(invisible(NULL))
  #}
  
  #if(x$RObj$d!=2 && (is.null(dim) 
  #  || length(dim)!=2)) stop("Image visualization only works for 2D data! Set dim in plot.") 
  
  #  mat <- x$RObj$toMatrix("transitional")
  mat <- x$RObj$toMatrix(grid_type, dim)
  
  if(!nrow(mat) || !ncol(mat)){
    warning("No clusters to plot!")
    return(invisible(NULL))
  }
  
  mat[mat==0] <- NA
  
  varnames <- attr(mat, "varnames")
  
  ### FIXME: this fails for a single grid!
  image(x=as.numeric(rownames(mat)), 
    y=as.numeric(colnames(mat)), 
    z=mat, 
    col=rev(gray.colors(100, alpha=1)), axes=TRUE, 
    xlab=varnames[1], ylab=varnames[2], main=main)
  
  if(!is.null(dsd)) {
    ps <- get_points(dsd, n=n, cluster=TRUE)
    pch <- attr(ps, "cluster")
    
    if(!is.null(dim)) ps <- ps[, dim]
    
    ### handle noise (samll circle)
    pch[is.na(pch)] <- .noise_pch
    points(ps, col= col_points, pch=pch)
  }
  
  ### add macro-clusters?
  if((type=="both" || type=="macro") && nclusters(x, type="macro") > 0) {
    points(get_centers(x, type="macro"), col="blue", lwd=2, pch=3, 
      cex=get_weights(x, type="macro", scale=c(1,5)))
  }
}

get_assignment.DSC_DStream <- function(dsc, points, type=c("auto", "micro", "macro"), 
  method=c("auto", "model", "nn"), ...) {
  
  type <- match.arg(type)
  method<- match.arg(method)
  
  if(method=="auto") method <- "model"
  if(method!="model") return(NextMethod())
  
  c <- get_centers(dsc, type="micro", ...)
  
  if(nrow(c)>0L) {
    dist <- dist(points, c, method="max")
    # Find the minimum distance and save the class
    assignment <- apply(dist, 1L, which.min)
    
    # dist>threshold means no assignment
    assignment[apply(dist, 1L, min) > dsc$RObj$gridsize/2] <- NA_integer_
    
  } else {
    warning("There are no clusters!")
    assignment <- rep(NA_integer_, nrow(points))
  }
  
  if(type=="macro") assignment <- microToMacro(dsc, assignment)
  
  attr(assignment, "method") <- "model"
  
  assignment 
}
