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

#' D-Stream Data Stream Clustering Algorithm
#'
#' Micro Clusterer with reclustering.
#' Implements the grid-based D-Stream data stream clustering algorithm.
#'
#' D-Stream creates an equally spaced grid and estimates the density in each
#' grid cell using the count of points falling in the cells. Grid cells are
#' classified based on density into dense, transitional and sporadic cells.
#' The density is faded after every new point by a factor of \eqn{2^{-lambda}}.
#' Every gaptime number of points sporadic grid cells are removed.
#'
#' For reclustering D-Stream (2007 version) merges adjacent dense grids to form
#' macro-clusters and then assigns adjacent transitional grids to
#' macro-clusters. This behavior is implemented as `attraction = FALSE`.
#'
#' The 2009 version of the algorithm adds the concept of attraction between
#' grids cells. If `attraction = TRUE` is used then the algorithm produces
#' macro-clusters based on attraction between dense adjacent grids (uses
#' `Cm2` which in the original algorithm is equal to `Cm`).
#'
#' For many functions (e.g., [get_centers()], [plot()]), D-Stream
#' adds a parameter `grid_type` with possible values of `"dense"`,
#' `"transitional"`, `"sparse"`, `"all"` and `"used"`. This
#' only returns the selected type of grid cells. `"used"` includes dense
#' and adjacent transitional cells which are used in D-Stream for reclustering.
#' For `plot()` D-Stream also provides extra parameters `"grid"` and
#' `"grid_type"` to show micro-clusters as grid cells (density represented
#' by gray values).
#'
#' `DSOutlier_DStream` classifies points that do not fall into a dense grid cell as
#' outlier/noise. Parameter `outlier_multiplier` specifies
#' how far the point needs to be away from a dense cell to be classified as an outlier by multiplying the grid
#' size.
#'
#' Note that `DSC_DStream` currently cannot be saved to disk using
#' save() or saveRDS(). This functionality will be added later!
#'
#' @aliases DSC_DStream dstream d-stream D-Stream
#' @family DSC_Micro
#' @family DSC_TwoStage
#' @family DSOutlier
#'
#' @param formula `NULL` to use all features in the stream or a model [formula] of the form `~ X1 + X2`
#'   to specify the features used for clustering. Only `.`, `+` and `-` are currently
#'   supported in the formula.
#' @param gridsize a single number defining the size of grid cells in each dimension.
#' For example for 3d data, the cells would be of size `gridsize x gridsize x gridsize`.
#' @param lambda Fading constant used function to calculate the decay factor
#' \eqn{2^-lambda}.  (Note: in the paper the authors use lamba to denote the
#' decay factor and not the fading constant!)
#' @param gaptime sporadic grids are removed every gaptime number of points.
#' @param Cm density threshold used to detect dense grids as a proportion of
#' the average expected density (Cm > 1). The average density is given by the
#' total weight of the clustering over \eqn{N}, the number of grid cells.
#' @param Cl density threshold to detect sporadic grids (0 > Cl > Cm).
#' Transitional grids have a density between Cl and Cm.
#' @param attraction compute and store information about the attraction between
#' adjacent grids. If `TRUE` then attraction is used to create
#' macro-clusters, otherwise macro-clusters are created by merging adjacent
#' dense grids.
#' @param epsilon overlap parameter for attraction as a proportion of
#' `gridsize`.
#' @param Cm2 threshold on attraction to join two dense grid cells (as a
#' proportion on the average expected attraction).  In the original algorithm
#' `Cm2` is equal to `Cm`.
#' @param k alternative to Cm2 (not in the original algorithm).  Create k
#' clusters based on attraction. In case of more than k unconnected components,
#' closer groups of MCs are joined.
#' @param N Fix the number of grid cells used for the calculation of the
#' density thresholds with Cl and Cm. If `N` is not given (0) then the
#' algorithm tries to determine N from the data. Note that this means that N
#' potentially increases over time and outliers might produce an extremely
#' large value which will lead to a sudden creation of too many dense
#' micro-clusters. The original paper assumed that N is known a priori.
#' @param x DSC_DStream object to get attraction values from.
#' @param relative calculates relative attraction (normalized by the cluster
#' weight).
#' @param grid_type the attraction between what grid types should be returned?
#' @param dist make attraction symmetric and transform into a distance.
#' @param outlier_multiplier multiplier for assignment grid width to declare outliers.
#' @return An object of class `DSC_DStream` (subclass of [DSC],
#' [DSC_R], [DSC_Micro]).
#' @author Michael Hahsler
#' @references
#' Yixin Chen and Li Tu. 2007. Density-based clustering for
#' real-time stream data. In _Proceedings of the 13th ACM SIGKDD
#' International Conference on Knowledge Discovery and Data Mining (KDD '07)._
#' ACM, New York, NY, USA, 133-142.
#'
#' Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density and
#' attraction. _ACM Transactions on Knowledge Discovery from Data,_ 3(3),
#' Article 12 (July 2009), 27 pages.
#' @examples
#' stream <- DSD_BarsAndGaussians(noise = .05)
#' plot(stream)
#'
#' dstream1 <- DSC_DStream(gridsize = 1, Cm = 1.5)
#' update(dstream1, stream, 1000)
#' dstream1
#'
#' # micro-clusters (these are "used" grid cells)
#' nclusters(dstream1)
#' head(get_centers(dstream1))
#'
#' # plot (DStream provides additional grid visualization)
#' plot(dstream1, stream)
#' plot(dstream1, stream, grid = TRUE)
#'
#' # look only at dense grids
#' nclusters(dstream1, grid_type = "dense")
#' plot(dstream1, stream, grid = TRUE, grid_type = "dense")
#'
#' # look at transitional and sparse cells
#' plot(dstream1, stream, grid = TRUE, grid_type = "transitional")
#' plot(dstream1, stream, grid = TRUE, grid_type = "sparse")
#'
#' ### Macro-clusters
#' # standard D-Stream uses reachability
#' nclusters(dstream1, type = "macro")
#' get_centers(dstream1, type = "macro")
#' plot(dstream1, stream, type = "macro")
#' evaluate_static(dstream1, stream, measure = "crand", type = "macro")
#'
#' # use attraction for reclustering
#' dstream2 <- DSC_DStream(gridsize = 1, attraction = TRUE, Cm = 1.5)
#' update(dstream2, stream, 1000)
#' dstream2
#'
#' plot(dstream2, stream, grid = TRUE)
#' evaluate_static(dstream2, stream, measure = "crand", type = "macro")
#' @export
DSC_DStream <- function(formula = NULL,
  gridsize,
  lambda = 1e-3,
  gaptime = 1000L,
  Cm = 3,
  Cl = .8,
  attraction = FALSE,
  epsilon = .3,
  Cm2 = Cm,
  k = NULL,
  N = 0)
  structure(
    list(
      description = "D-Stream",
      formula = formula,
      RObj = dstream$new(gridsize, lambda,
        gaptime, Cm, Cl, attraction, epsilon,
        Cm2, k, N),
      macro = DSC_Static(x = list(centers = data.frame()), type = "macro")
    ),
    class = c("DSC_DStream", "DSC_Micro", "DSC_R", "DSC")
  )

dstream <- setRefClass(
  "dstream",
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
    CppObj 		      = "ANY",
    serial          = "ANY",
    decay_factor		= "numeric",

    ### column names for centers
    colnames      = "ANY",

    ### do we need to rerun the reclusterer
    newdata         = "logical"
  ),

  methods = list(
    initialize = function(gridsize = 0.1,
      lambda   = 1e-3,
      gaptime  = 1000L,
      Cm       = 3,
      Cl       = .8,
      attraction = FALSE,
      epsilon  = .3,
      Cm2      = 3,
      k        = NULL,
      N        = 0) {
      gridsize  <<- gridsize
      lambda    <<- lambda
      gaptime	  <<- as.integer(gaptime)
      Cm        <<- Cm
      Cl        <<- Cl
      attraction <<- as.logical(attraction)
      epsilon   <<- epsilon
      Cm2       <<- Cm2
      N         <<- N

      colnames  <<- NULL
      newdata  <<- TRUE

      if (is.null(k))
        k <<- 0L
      else
        k <<- as.integer(k)

      if (N == 0)
        N_fixed <<- FALSE
      else
        N_fixed <<- TRUE

      ### this is what the paper calls lambda!
      decay_factor <<- 2 ^ (-lambda)

      CppObj <<- new(DStream,
        gridsize,
        decay_factor,
        gaptime,
        Cl,
        N,
        attraction,
        epsilon)

      .self
    }
  )
)

dstream$methods(
  list(
    # overload copy
    copy = function(...) {
      #callSuper(...)
      ### copy S4 object
      n <- dstream$new(gridsize, decay_factor, gaptime,
        Cl, N, attraction, epsilon)

      ### copy Rcpp object
      n$CppObj <- new(DStream, CppObj$serializeR())

      n
    },

    cache = function() {
      serial <<- CppObj$serializeR()
    },

    uncache = function() {
      CppObj <<- new(DStream, serial)
      serial <<- NULL
      newdata <<- TRUE
    },


    cluster = function(newdata, debug = FALSE) {
      'Cluster new data.' ### online help

      newdata <<- TRUE
      CppObj$update(as.matrix(newdata), debug)

      ### assignment is not implemented.
      NULL
    },

    ### This is for plotting images.
    toMatrix = function(grid_type = "used", dim = NULL) {
      ### nothing clustered yet
      if (!length(CppObj$mins) ||
          !length(CppObj$maxs))
        return(matrix(0, nrow = 0, ncol = 0))

      cs <-
        get_micro(weight = TRUE,
          grid_type = grid_type,
          translate = FALSE)
      ws <- attr(cs, "weight")

      ns <- (CppObj$maxs - CppObj$mins) + 1L
      mat <- matrix(0, nrow = ns[1], ncol = ns[2])

      if (nrow(cs) > 0) {
        ## dimensions to show
        if (!is.null(dim))
          cs <- cs[, dim[1:2], drop = FALSE]
        else
          cs <- cs[, 1:2, drop = FALSE]

        for (i in seq_len(nrow(cs)))
          mat[cs[i, 1] - CppObj$mins[1] + 1,
            cs[i, 2] - CppObj$mins[2] + 1] <- ws[i]
      }

      rownames(mat) <-
        (CppObj$mins[1]:CppObj$maxs[1]) * gridsize + gridsize / 2
      colnames(mat) <-
        (CppObj$mins[2]:CppObj$maxs[2]) * gridsize + gridsize / 2
      ### FIXME: Colnames!
      attr(mat, "varnames") <- colnames(cs)

      mat
    },

    get_attraction = function(relative = FALSE,
      grid_type = "dense",
      dist = FALSE) {
      if (!attraction)
        stop("No attraction values stored. ",
          "Create the DSC_DStream with attraction=TRUE.")

      if (dist)
        stop("dist not implemented yet...")

      attr_matrix <-  CppObj$getAttraction()

      if (relative) {
        w <- attr(get_micro(weight = TRUE), "weight")
        attr_matrix <- attr_matrix / w
      }

      c_type <- mc_type(grid_type)
      used <- attr(c_type, "used")
      attr_matrix <- attr_matrix[used, used, drop = FALSE]

      if (dist)
        as.dist(attr_matrix) ### FIXME: make symetric first
      else
        attr_matrix
    },

    get_micro = function(weight = FALSE,
      cluster_type = FALSE,
      translate = TRUE,
      grid_type = c("used", "dense", "transitional", "sparse", "all")) {
      grid_type <- match.arg(grid_type)

      cs <- data.frame(CppObj$centers(FALSE))  # no decoding
      ws <- CppObj$weights()

      if (length(ws) == 0) {
        ret <- data.frame()
        if (weight)
          attr(ret, "weight") <- numeric(0)
        if (cluster_type)
          attr(ret, "cluster_type") <- factor(levels =
              c("dense", "transitional", "sparse"))
        else
          return(ret)
      }

      c_type <- mc_type(grid_type) ## updates N
      used <- attr(c_type, "used")
      cs <- cs[used, , drop = FALSE]
      ws <- ws[used]
      c_type <- c_type[used]

      ### translate coordinates?
      if (translate && nrow(cs) > 0)
        cs <- cs * gridsize + gridsize / 2

      if (weight)
        attr(cs, "weight") <- ws
      if (cluster_type)
        attr(cs, "cluster_type") <- c_type
      rownames(cs) <- NULL
      cs
    },

    mc_type = function(grid_type) {
      cs <- CppObj$centers(FALSE)  # no decoding
      ws <- CppObj$weights()

      ## update N?
      if (!N_fixed)
        N <<- prod(CppObj$maxs - CppObj$mins + 1L)

      c_type <- factor(rep.int("sparse", times = length(ws)),
        levels = c("dense", "transitional", "sparse"))
      c_type[ws > Cl / (N * (1 - decay_factor))] <- "transitional"
      c_type[ws > Cm / (N * (1 - decay_factor))] <- "dense"

      if (grid_type == "all") {
        used <- rep(TRUE, time = length(c_type))
      } else if (grid_type != "used") {
        used <- c_type == grid_type
      } else{
        # dense + adjacent transitional
        dense <- c_type == "dense"
        trans <- c_type == "transitional"
        used <- dense

        if (any(dense) && any(trans)) {
          take <-
            dist(cs[trans, , drop = FALSE], cs[dense, , drop = FALSE], method = "Manhattan") <= 1
          take <- apply(take, 1L, any)
          used[which(trans)[take]] <- TRUE
        }
      }

      attr(c_type, "used") <- used
      c_type
    },

    get_microclusters = function(...) {
      centers <- data.frame(get_micro(...))
      colnames(centers) <- colnames
      centers
    },

    get_microweights = function(...) {
      attr(get_micro(weight = TRUE, ...), "weight")
    },

    get_macro_clustering = function(...) {
      mcs <- get_micro(
        grid_type = "used",
        translate = FALSE,
        weight = TRUE,
        cluster_type = TRUE
      )
      ws <- attr(mcs, "weight")
      c_type <- attr(mcs, "cluster_type")

      ### no mcs
      if (nrow(mcs) < 1)
        return(list(
          centers = empty_df(colnames),
          microToMacro = integer(0L),
          weights = numeric(0L)
        ))


      ### single mc
      if (nrow(mcs) == 1)
        return(list(
          centers = mcs,
          weights = ws,
          microToMacro = structure(1L, names = "1")
        ))

      denseID <- c_type == "dense"
      dense <- mcs[denseID, , drop = FALSE]
      transID <- c_type == "transitional"
      trans <- mcs[transID, , drop = FALSE]

      if (attraction) {
        ### use attraction
        a <- get_attraction(grid_type = "dense")

        if (nrow(a) > 1) {
          d_attr <- as.dist(-a - t(a))

          if (k > 0L)  {
            ### use k?

            hc <- hclust(d_attr, method = "single")
            ### find unconnected components
            assignment <- cutree(hc, h = 0 - 1e-9)

            maxk <- min(k, nrow(a))
            ### not enough components?
            if (length(unique(assignment)) < maxk)
              assignment <- cutree(hc, k = maxk)

            ### FIXME: If k>number of connected components then components would
            ###  be merged randomly! So we add for these the regular distance!

            #d_dist <- dist(mcs)
            #unconnected <- d_attr==0 ### an attraction count of 0!
            #d_attr[unconnected] <- d_attr[unconnected] + d_dist[unconnected]
            #assignment <- cutree(hclust(d_attr, method="single"), k=k)

          } else{
            ### use Cm2

            P <-
              2 * sum(CppObj$maxs - CppObj$mins) ### number of possible attraction values
            ### actually we should check each direction independently
            assignment <- cutree(hclust(d_attr, method = "single"),
              h = -2 * Cm2 / P / (1 + decay_factor))
          }
        } else
          assignment <- 1L

      } else{
        ### use adjacency
        if (nrow(dense) > 1) {
          d_pos <- dist(dense)
          assignment <- cutree(hclust(d_pos, method = "single"),
            h = 1.1) ### anything less than 2^.5 is fine
        } else
          assignment <- 1L
      }

      ### assign transitional grids
      if (nrow(trans) > 0) {
        ass <- rep.int(NA_integer_, length(c_type))
        ass[denseID] <- assignment

        # this assigns it to one of the neighboring macro clusters
        take <- dist(trans, dense, method = "Manhattan") <= 1
        take <- apply(
          take,
          1L,
          FUN = function(x)
            which(x)[1]
        )
        ass[transID] <- assignment[take]
        assignment <- ass
      }

      ### translate mcs
      mcs <- mcs * gridsize + gridsize / 2

      m2m <- structure(assignment, names = seq_along(assignment))

      ### find centroids
      macro <- .centroids(mcs, ws, m2m)
      macro$microToMacro <- m2m

      colnames(macro$centers) <- colnames
      macro
    }
  )
)

#' @rdname DSC_DStream
#' @export
get_attraction <-
  function(x,
    relative = FALSE,
    grid_type = "dense",
    dist = FALSE)
    x$RObj$get_attraction(relative = relative,
      grid_type = grid_type,
      dist = dist)


# helper to memorize macro clusterings
.dstream_update_macro <- function(x) {
  if (!x$RObj$newdata)
    return()

  cluster_list <- x$RObj$get_macro_clustering()
  x$macro$RObj$centers <- cluster_list$centers
  x$macro$RObj$weights <- cluster_list$weights
  x$macro$RObj$microToMacro <- cluster_list$microToMacro
  x$RObj$newdata <- FALSE
}


#' @export
get_macroclusters.DSC_DStream <- function(x, ...) {
  .dstream_update_macro(x)
  get_centers(x$macro)
}

#' @export
get_macroweights.DSC_DStream <- function(x, ...) {
  .dstream_update_macro(x)
  get_weights(x$macro)
}

#' @export
microToMacro.DSC_DStream <- function(x, micro = NULL, ...) {
  .nodots(...)
  .dstream_update_macro(x)
  assignment <- x$macro$RObj$microToMacro

  if (!is.null(micro))
    assignment <- assignment[micro]
  assignment
}

### add plot as a grid
#' @rdname DSC_DStream
#' @param dsd a [DSD] data stream object.
#' @param n number of plots taken from `dsd` to plot.
#' @param type Plot micro clusters (`type = "micro"`), macro clusters (`type = "macro"`),
#'   both micro and macro clusters (`type = "both"`), outliers(`type = "outliers"`),
#'   or everything together (`type = "all"`). `type = "auto"` leaves to the class of DSC to decide.
#' @param assignment logical; show assignment area of micro-clusters.
#' @param grid logical; show the D-Stream grid instead of circles for micro-clusters.
#' @param ... further argument are passed on.
#' @export
plot.DSC_DStream <- function(x,
  dsd = NULL,
  n = 500,
  type = c("auto", "micro", "macro", "both"),
  grid = FALSE,
  grid_type = "used",
  assignment = FALSE,
  ...) {
  ### find type
  dim <- list(...)$dim
  main <- list(...)$main

  ### grid uses a darker color for the points
  col_points <- list(...)$col_points
  if (is.null(col_points))
    col_points <- gray(.1, alpha = .3)

  type <- match.arg(type)
  if (type == "auto")
    type <- "both"

  ### assignment == grid
  if (assignment)
    grid <- TRUE

  ### implements grid and grid_both
  if (!grid)
    return(plot.DSC(
      x,
      dsd = dsd,
      n = n,
      type = type,
      ...
    ))

  #if(nclusters) {
  #  warning("No clusters to plot!")
  #  return(invisible(NULL))
  #}

  #if(x$RObj$d!=2 && (is.null(dim)
  #  || length(dim)!=2)) stop("Image visualization only works for 2D data! Set dim in plot.")

  #  mat <- x$RObj$toMatrix("transitional")
  mat <- x$RObj$toMatrix(grid_type, dim)

  if (!nrow(mat) || !ncol(mat)) {
    warning("No clusters to plot!")
    return(invisible(NULL))
  }

  mat[mat == 0] <- NA
  varnames <- attr(mat, "varnames")

  ### get varnames from data stream
  if (!is.null(dsd)) {
    ps <- get_points(dsd,
      n = n,
      info = FALSE)
    varnames <- colnames(ps)
  }

  ### FIXME: this fails for a single grid!
  image(
    x = as.numeric(rownames(mat)),
    y = as.numeric(colnames(mat)),
    z = mat,
    col = rev(gray.colors(100, alpha = 1)),
    axes = TRUE,
    xlab = varnames[1],
    ylab = varnames[2],
    main = main
  )

  if (!is.null(dsd)) {
    pch <- attr(ps, "cluster")

    if (!is.null(dim))
      ps <- ps[, dim]

    ### handle noise (small circle)
    pch[is.na(pch)] <- .noise_pch
    points(ps, col = col_points, pch = pch)
  }

  ### add macro-clusters?
  if ((type == "both" ||
      type == "macro") && nclusters(x, type = "macro") > 0) {
    centers <-  get_centers(x, type = "macro")
    if(!is.null(dim))
      centers <- centers[, dim, drop = FALSE]

    points(
      centers,
      col = "blue",
      lwd = 2,
      pch = 3,
      cex = get_weights(x, type = "macro", scale = c(1, 5))
    )
  }
}

#' @export
get_assignment.DSC_DStream <-
  function(dsc,
    points,
    type = c("auto", "micro", "macro"),
    method = c("auto", "model", "nn"),
    ...) {
    type <- match.arg(type)
    method <- match.arg(method)

    points <- remove_info(points)

    ## apply formula
    if (!is.null(dsc$RObj$colnames))
      points <- points[, dsc$RObj$colnames, drop = FALSE]

    if (method == "auto")
      method <- "model"
    if (method != "model")
      return(NextMethod())

    c <- get_centers(dsc, type = "micro", ...)

    if (nrow(c) > 0L) {
      dist <- dist(points, c, method = "max")
      # Find the minimum distance and save the class
      assignment <- apply(dist, 1L, which.min)

      # dist>threshold means no assignment
      #assignment[apply(dist, 1L, min) > dsc$RObj$gridsize / 2] <-
      #  NA_integer_

      # If we have an outlier_multiplier then we increase the radius
      r_multiplier <- 1
      if (!is.null(dsc$outlier_multiplier))
        r_multiplier <-
        dsc$outlier_multiplier
      assignment[apply(dist, 1L, min) > dsc$RObj$gridsize / 2 * r_multiplier] <-
        NA_integer_

    } else {
      #warning("There are no clusters!")
      assignment <- rep(NA_integer_, nrow(points))
    }

    if (type == "macro")
      assignment <- microToMacro(dsc, assignment)

    attr(assignment, "method") <- "model"

    assignment
  }

#' @rdname DSC_DStream
#' @export
DSOutlier_DStream <- function(formula = NULL,
  gridsize,
  lambda = 1e-3,
  gaptime = 1000L,
  Cm = 3,
  Cl = .8,
  outlier_multiplier = 2) {
  cl <- DSC_DStream(formula, gridsize, lambda, gaptime, Cm, Cl)
  class(cl) <- c("DSOutlier", class(cl))

  cl$outlier_multiplier <- outlier_multiplier

  cl
  }
