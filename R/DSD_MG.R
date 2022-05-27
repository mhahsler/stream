#######################################################################
# Moving Generator -  Infrastructure for Moving Streams
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

#' DSD Moving Generator
#'
#' Creates an evolving DSD that consists of several [MGC], each representing a moving cluster.
#'
#' This DSD is able to generate complex datasets that are able to evolve over a
#' period of time. Its behavior is determined by as set of [MGC]s, each representing
#' a moving cluster.
#'
#' @family DSD
#'
#' @param dimension the dimension of the DSD object
#' @param ... initial set of [MGC]s
#' @param x A `DSD_MG` object.
#' @param c The cluster that should be added to the `DSD_MG` object.
#' @param i The index of the cluster that should be removed from the
#' `DSD_MG` object.
#' @param label,labels integer representing the cluster label. `NA`
#' represents noise.  If labels are not specified, then each new cluster gets a
#' new label.
#' @param description An optional string used by `print()` to describe the
#' data generator.
#' @seealso [MGC] for types of moving clusters.
#' @author Matthew Bolanos
#' @examples
#' ### create an empty DSD_MG
#' stream <- DSD_MG(dim = 2)
#' stream
#'
#' ### add two clusters
#' c1 <- MGC_Random(density = 50, center = c(50, 50), parameter = 1)
#' add_cluster(stream, c1)
#' stream
#'
#' c2 <- MGC_Noise(density = 1, range = rbind(c(-20, 120), c(-20, 120)))
#' add_cluster(stream, c2)
#' stream
#'
#' get_clusters(stream)
#' get_points(stream, n = 5, info = TRUE)
#' plot(stream, xlim = c(-20,120), ylim = c(-20, 120))
#'
#' if (interactive()) {
#' animate_data(stream, n = 5000, xlim = c(-20, 120), ylim = c(-20, 120))
#' }
#'
#' ### remove cluster 1
#' remove_cluster(stream, 1)
#'
#' get_clusters(stream)
#' plot(stream, xlim = c(-20, 120), ylim = c(-20, 120))
#'
#' ### create a more complicated cluster structure (using 2 clusters with the same
#' ### label to form an L shape)
#' stream <- DSD_MG(dim = 2,
#'   MGC_Static(density = 10, center = c(.5, .2),   par = c(.4, .2),   shape = MGC_Shape_Block),
#'   MGC_Static(density = 10, center = c(.6, .5),   par = c(.2, .4),   shape = MGC_Shape_Block),
#'   MGC_Static(density = 5,  center = c(.39, .53), par = c(.16, .35), shape = MGC_Shape_Block),
#'   MGC_Noise( density = 1,  range = rbind(c(0,1), c(0,1))),
#'   labels = c(1, 1, 2, NA)
#'   )
#'
#' plot(stream, xlim = c(0, 1), ylim = c(0, 1))
#'
#' ### simulate the clustering of a splitting cluster
#' c1 <- MGC_Linear(dim = 2, keyframelist = list(
#'   keyframe(time = 1,  dens = 20, center = c(0,0),   param = 10),
#'   keyframe(time = 50, dens = 10, center = c(50,50), param = 10),
#'   keyframe(time = 100,dens = 10, center = c(50,100),param = 10)
#' ))
#'
#' ### Note: Second cluster appearch at time=50
#' c2 <- MGC_Linear(dim = 2, keyframelist = list(
#'   keyframe(time = 50, dens = 10, center = c(50,50), param = 10),
#'   keyframe(time = 100,dens = 10, center = c(100,50),param = 10)
#' ))
#'
#' stream <- DSD_MG(dim = 2, c1, c2)
#' stream
#'
#' dbstream <- DSC_DBSTREAM(r = 10, lambda = 0.1)
#' if (interactive()) {
#' purity <- animate_cluster(dbstream, stream, n = 2500, type = "micro",
#'   xlim = c(-10, 120), ylim = c(-10, 120), evaluationMeasure = "purity", horizon = 100)
#' }
#' @export
DSD_MG <-
  function(dimension = 2,
    ...,
    labels = NULL,
    description = NULL) {
    if (is.null(description))
      description <- "Moving Data Generator"

    x <- structure(
      list(description = description,
        RObj = dsd_MG_refClass$new(d = dimension)),
      class = c("DSD_MG", "DSD_R", "DSD")
    )

    l <- list(...)
    if (length(l) > 0) {
      for (i in 1:length(l)) {
        add_cluster(x, l[[i]], labels[i])
      }
    }

    x
  }

#' @rdname DSD_MG
#' @export
add_cluster <- function(x, c, label = NULL)
  UseMethod("add_cluster")

#' @rdname DSD_MG
#' @export
get_clusters <- function(x)
  UseMethod("get_clusters")

#' @rdname DSD_MG
#' @export
remove_cluster <- function(x, i)
  UseMethod("remove_cluster")

dsd_MG_refClass <- setRefClass(
  "dsd_MG",
  fields = list(
    t = "numeric",
    dimension = "numeric",
    clusters = "list",
    labels = "integer"
  ),
  methods = list(
    initialize = function(d) {
      t <<- 1
      dimension  <<- d
      clusters <<- list()
      labels <<- integer(0)
      .self
    }

  ),
)

dsd_MG_refClass$methods(
  add_cluster = function(c, label = NULL) {
    if (c$RObj$dimension != dimension)
      stop("Cluster dimensions do not match!")
    clusters <<- append(clusters, list(c))

    if (is.null(label)) {
      if (length(labels) == 0)
        label <- 1L
      else
        label <- max(labels, na.rm = TRUE) + 1L
    } else
      label <- as.integer(label)
    labels <<- append(labels, label)
  },

  get_points = function(n,
    info = FALSE) {
    if (length(clusters) == 0)
      stop("DSD_MG does not contain any clusters!")

    # only allocate if cluster info is requested
    if (info)
      a <- integer(n)

    data <- matrix(NA_real_, nrow = n, ncol = dimension)

    j <- 0L
    while (j < n) {
      density <- unlist(sapply(clusters,
        function(x)
          x$RObj$get_attributes(t, "density")))

      density[is.na(density)] <- 0
      if (all(density == 0))
        stop("No MGC is producing points for this time point.")

      pointsPerSecond <- sum(density)
      pointsLeftInSecond <-
        pointsPerSecond - (t - floor(t)) * pointsPerSecond
      if ((j + pointsLeftInSecond) <= n)
        k <- pointsLeftInSecond
      else
        k <- n - j

      ### got to next timestep...
      if (pointsLeftInSecond < 1) {
        t <<- ceiling(t)
        next
      }

      k <- floor(k)

      if (k >= 1) {
        clusterOrder <- sample(
          x = 1:length(clusters),
          size = k,
          replace = TRUE,
          prob = density / sum(density)
        )

        data[(j + 1):(j + k), ] <-
          t(sapply(
            clusterOrder,
            FUN = function(i) {
              clusters[[i]]$RObj$get_points(t)
            }
          ))

        if (info)
          a[(j + 1):(j + k)] <- labels[clusterOrder]
      }

      t <<- t + k / pointsPerSecond
      j <- j + k
    }

    data <- data.frame(data)

    if (info)
      data[['.class']] <- a

    data
  }
)

#' @export
get_points.DSD_MG <- function(x,
  n = 1,
  outofpoints = "stop",
  info = FALSE,
  ...) {
  .nodots(...)

  x$RObj$get_points(n, info = info)
}

#' @rdname DSD_MG
#' @export
add_cluster.DSD_MG <- function(x, c, label = NULL) {
  ### name noise NA unless specified otherwise
  if (is.null(label) && is(c, "MGC_Noise"))
    label <- NA
  x$RObj$add_cluster(c, label)
}

#' @export
reset_stream.DSD_MG <- function(dsd, pos = 1) {
  dsd$RObj$t <- pos
}

#' @export
print.DSD_MG <- function(x, ...) {
  #NextMethod()
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse = ", "), "\n")
  cat(
    paste(
      'With',
      length(na.omit(unique(x$RObj$labels))),
      'clusters',
      'in',
      x$RObj$dimension,
      'dimensions. Time is',
      round(x$RObj$t, 3),
      '\n'
    )
  )
}

#' @export
get_clusters.DSD_MG <- function(x) {
  x$RObj$clusters
}

#' @export
remove_cluster.DSD_MG  <- function(x, i) {
  x$RObj$clusters[[i]] <- NULL
  x$RObj$labels <- x$RObj$labels[-i]
}
