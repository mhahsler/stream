#' Plot Results of a Data Stream Clustering
#'
#' Method to plot the result of data stream data clustering. To plot [DSD] see [plot.DSD()].
#'
#' @family DSC
#' @family plot
#'
#' @param x the [DSC] object to be plotted.
#' @param dsd a [DSD] object to plot the data in the background.
#' @param n number of plots taken from `dsd` to plot.
#' @param col_points,col_clusters colors used for plotting.
#' @param weights if `TRUE` then the cluster weight is used for symbol size. Alternatively, a
#'   vector with the size of the symbols for micro- and macro-clusters can be supplied.
#' @param scale range for the symbol sizes used.
#' @param cex size factor for symbols.
#' @param pch symbol type for points.
#' @param method method used for plotting: `"pairs"` (pairs plot), `"scatter"`
#' (scatter plot), `"pca"` (plot first 2 principal components).
#' @param dim an integer vector with the dimensions to plot. If `NULL`
#' then for methods `pairs` and `"pca"` all dimensions are used and
#' for `"scatter"` the first two dimensions are plotted.
#' @param type Plot micro clusters (`type = "micro"`), macro clusters
#' (`type = "macro"`), both micro and macro clusters (`type = "both"`).
#' @param assignment logical; show assignment area of micro-clusters.
#' @param \dots further arguments are passed on to [graphics::plot.default()] or [graphics::pairs()].
#' \pkg{graphics}.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k=3, d=3)
#'
#' ## create and plot micro-clusters
#' dstream <- DSC_DStream(gridsize = 0.1)
#' update(dstream, stream, 500)
#' plot(dstream)
#'
#' ## plot with data, projected on the first two principal components
#' ## and dimensions 2 and 3
#' plot(dstream, stream)
#' plot(dstream, stream, method = "pca")
#' plot(dstream, stream, dim = c(2, 3))
#'
#' ## plot micro and macro-clusters
#' plot(dstream, stream, type = "both")
#'
#' ## show assignment area
#' plot(dstream, stream, type = "both", assignment = TRUE)
#' @export
plot.DSC <- function(x,
  dsd = NULL,
  n = 500,
  col_points = NULL,
  col_clusters = c("red", "blue", "green"),
  weights = TRUE,
  scale = c(1, 5),
  cex = 1,
  pch = NULL,
  method = c("pairs", "scatter", "pca"),
  dim = NULL,
  type = c("auto", "micro", "macro", "both"),
  # we keep 'both' for compatibility reasons
  assignment = FALSE,
  ### assignment is not implemented
  ...) {
  type <- match.arg(type)
  method <- match.arg(method)

  if (is.null(col_points))
    col_points <- .points_col

  if (type == "auto")
    type <- get_type(x)

  ## plot micro or macro clusters
  if (type != "both") {
    ## method can be pairs, scatter or pca
    centers <- get_centers(x, type = type)
    k <- nrow(centers)

    if (k < 1) {
      warning("No clusters to plot!")
      plot(NA, NA, xlim = c(0, 0), ylim = c(0, 0))
      return()
    }

    if (weights)
      cex_clusters <- get_weights(x, type = type, scale = scale)
    else
      cex_clusters <- rep(1, k)

    if (type == "micro") {
      col <- rep(col_clusters[1], k)
      mpch <- rep(1, k)
      lwd <- rep(1, k)
    } else if (type == "macro") {
      cex_clusters <- cex_clusters * 1.5
      col <- rep(col_clusters[2], k)
      mpch <- rep(3, k)
      lwd <- rep(2, k)
    }

  } else {
    ### both
    centers_mi <- get_centers(x, type = "micro")
    centers_ma <- get_centers(x, type = "macro")
    centers_out <- data.frame()

    k_mi <- nrow(centers_mi)
    k_ma <- nrow(centers_ma)

    if (k_mi < 1L) {
      warning("No clusters to plot!")
      plot(NA, NA, xlim = c(0, 0), ylim = c(0, 0))
      return()
    }

    ### fix names if necessary
    colnames(centers_mi) <- colnames(centers_ma)
    centers <- rbind(centers_mi, centers_ma)

    if (weights)
      cex_clusters <- c(
        get_weights(x, type = "micro", scale = scale),
        get_weights(x, type = "macro", scale = scale * 1.5)
      )
    else
      cex_clusters <-
      c(rep(cex, k_mi), rep(cex * 2, k_ma))

    col <-
      c(rep(col_clusters[1], k_mi),
        rep(col_clusters[2], k_ma))
    mpch <- c(rep(1, k_mi), rep(3, k_ma))
    lwd <- c(rep(1, k_mi), rep(2, k_ma))
  }

  ### prepend data if given so it is in the background
  if (!is.null(dsd)) {
    d <- get_points(dsd, n, info = FALSE)
    #	names(d) <- names(centers)
    # fix center names
    colnames(centers) <- colnames(d)
    centers <- rbind(d, centers)


    col <- c(rep(col_points[1], n), col)
    cex_clusters <- c(rep(cex[1], n), cex_clusters)
    ## TODO: we could use cluster labels for pch
    mpch <- c(rep(1L, n), mpch)

    ## cannot be more than 25
    mpch <- mpch %% 25
    lwd <- c(rep(1, n), lwd)

    ### handle noise
    noise <- is.na(mpch)
    mpch[noise] <- .noise_pch
    col[noise] <- .noise_col
    #cex_clusters[noise] <- cex_clusters[noise]*.5

  }

  if (!is.null(pch))
    mpch <- pch

  if (!is.null(dim))
    centers <- centers[, dim]

  ### plot
  if (method == "pairs" &&  ncol(centers) > 2L) {
    pairs(
      centers,
      col = col,
      cex = cex_clusters,
      pch = mpch,
      lwd = lwd,
      ...
    )
  }
  else if (method == "pca" && ncol(centers) > 2L) {
    ## we assume Euclidean here
    p <- prcomp(centers)
    plot(
      p$x,
      col = col,
      cex = cex_clusters,
      pch = mpch,
      lwd = lwd,
      ...
    )
  } else if (ncol(centers) == 1L) {
    plot(
      centers[[1]],
      rep(0, length(centers[[1]])),
      col = col,
      cex = cex_clusters,
      pch = mpch,
      lwd = lwd,
      ylab = "",
      xlab = colnames(centers)[1],
      ...
    )
  } else {
    ## plot first 2 dimensions
    if (ncol(centers) > 2L)
      centers <- centers[, 1:2]
    plot(
      centers,
      col = col,
      cex = cex_clusters,
      pch = mpch,
      lwd = lwd,
      ...
    )
  }
}
