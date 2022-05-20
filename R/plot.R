#' Plotting Data Stream Data and Clusterings
#'
#' Methods to plot data stream data and clusterings.
#'
#' @family DSC
#' @family DSD
#'
#' @name plot
#'
#' @param x the [DSD] or [DSC] object to be plotted.
#' @param dsd a [DSD] object to plot the data in the background if `x` is a [DSC].
#' @param n number of plots taken from the dsd to plot.
#' @param col,col_points,col_clusters colors used for plotting.
#' @param weights the size of the symbols for micro- and macro-clusters
#' represents its weight.
#' @param scale range for the symbol sizes used.
#' @param cex size factor for symbols.
#' @param pch symbol type.
#' @param method method used for plotting: `"pairs"` (pairs plot), `"scatter"`
#' (scatter plot), `"pca"` (plot first 2 principal components), or `"ts"` (time series).
#' @param dim an integer vector with the dimensions to plot. If `NULL`
#' then for methods `pairs` and `"pca"` all dimensions are used and
#' for `"scatter"` the first two dimensions are plotted.
#' @param alpha alpha shading used to plot the points.
#' @param type Plot micro clusters (`type = "micro"`), macro clusters
#' (`type = "macro"`), both micro and macro clusters (`type = "both"`),
#' outliers(`type = "outliers"`), or everything together
#' (`type = "all"`). `type = "auto"` leaves to the class of DSC to
#' decide.
#' @param assignment logical; show assignment area of micro-clusters.
#' @param \dots further arguments are passed on to [plot] or [pairs] in
#' \pkg{graphics}.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k=3, d=3)
#'
#' ## plot data
#' plot(stream, n=500)
#' plot(stream, method="pca", n=500)
#' plot(stream, method="scatter", dim=c(1,3), n=500)
#'
#' ## create and plot micro-clusters
#' dstream <- DSC_DStream(gridsize=0.1)
#' update(dstream, stream, 500)
#' plot(dstream)
#'
#' ## plot with data, projected on the first two principal components
#' ## and dimensions 2 and 3
#' plot(dstream, stream)
#' plot(dstream, stream, method="pc")
#' plot(dstream, stream, dim=c(2,3))
#'
#' ## plot micro and macro-clusters
#' plot(dstream, stream, type="both")

#' @rdname plot
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
  method = "pairs",
  dim = NULL,
  type = c("auto", "micro", "macro", "both", "all", "outliers"),
  # we keep 'both' for compatibility reasons
  assignment = FALSE,
  ### assignment is not implemented
  ...) {
  type <- match.arg(type)
  if (type == "outliers" && !is(x, "DSOutlier"))
    stop("The clusterer is not an outlier detector, cannot draw outliers")

  if (is.null(col_points))
    col_points <- .points_col

  if (type != "both" && type != "all") {
    if (type == "auto")
      type <- get_type(x)
    ## method can be pairs, scatter or pc (projection with PCA)
    if (type != "outliers") {
      centers <- get_centers(x, type = type)
    } else {
      centers <- get_outlier_positions(x)
    }
    k <- nrow(centers)

    if (k < 1) {
      warning("No clusters or outliers to plot!")
      plot(NA, NA, xlim = c(0, 0), ylim = c(0, 0))
      return()
    }

    if (type != "outliers") {
      if (weights)
        cex_clusters <- get_weights(x, type = type, scale = scale)
      else
        cex_clusters <- rep(1, k)
    } else
      cex_clusters <- rep(2, k)

    if (type == "micro") {
      col <- rep(col_clusters[1], k)
      mpch <- rep(1, k)
      lwd <- rep(1, k)
    } else if (type == "macro") {
      cex_clusters <- cex_clusters * 1.5
      col <- rep(col_clusters[2], k)
      mpch <- rep(3, k)
      lwd <- rep(2, k)
    } else {
      col <- rep(col_clusters[3], k)
      mpch <- rep(1, k)
      lwd <- rep(1, k)
    }
  } else {
    ### both
    centers_mi <- get_centers(x, type = "micro")
    centers_ma <- get_centers(x, type = "macro")
    centers_out <- data.frame()
    if (type == "all" &&
        is(x, "DSOutlier"))
      centers_out <- get_outlier_positions(x)
    k_mi <- nrow(centers_mi)
    k_ma <- nrow(centers_ma)
    k_out <- nrow(centers_out)

    if ((k_mi + k_out) < 1) {
      warning("No clusters or outliers to plot!")
      plot(NA, NA, xlim = c(0, 0), ylim = c(0, 0))
      return()
    }

    ### Fix names if necessary
    colnames(centers_mi) <- colnames(centers_ma)
    if (nrow(centers_out) > 0)
      colnames(centers_out) <- colnames(centers_ma)

    centers <- rbind(centers_mi, centers_ma, centers_out)

    if (weights)
      cex_clusters <- c(
        get_weights(x, type = "micro", scale = scale),
        get_weights(x, type = "macro", scale = scale * 1.5),
        rep(2, k_out)
      )
    else
      cex_clusters <-
      c(rep(cex, k_mi), rep(cex * 2, k_ma), rep(2, k_out))

    col <-
      c(rep(col_clusters[1], k_mi),
        rep(col_clusters[2], k_ma),
        rep(col_clusters[3], k_out))
    mpch <- c(rep(1, k_mi), rep(3, k_ma), rep(1, k_out))
    lwd <- c(rep(1, k_mi), rep(2, k_ma), rep(1, k_out))
  }

  ### prepend data if given
  if (!is.null(dsd)) {
    d <- get_points(dsd, n, cluster = TRUE, outlier = TRUE)
    #	names(d) <- names(centers)
    # fix center names
    colnames(centers) <- colnames(d)
    centers <- rbind(d, centers)

    col <- c(rep(col_points, n)[1:n], col)
    cex_clusters <- c(rep(cex, n), cex_clusters)
    mpch <- c(attr(d, "cluster"), mpch)
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
  if (ncol(centers) > 2 && method == "pairs") {
    pairs(
      centers,
      col = col,
      cex = cex_clusters,
      pch = mpch,
      lwd = lwd,
      ...
    )
  }
  else if (ncol(centers) > 2 && method == "pc") {
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
  } else if (ncol(centers) == 1) {
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
    if (ncol(centers) > 2)
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


#' @rdname plot
#' @export
plot.DSD <- function(x,
  n = 500,
  col = NULL,
  pch = NULL,
  ...,
  method = "pairs",
  dim = NULL,
  alpha = .6) {
  method <- match.arg(method, c('scatter', 'pairs', 'pca', 'ts'))

  ## method can be pairs, plot or pc (projection with PCA)

  d <- get_points(x, n, cluster = TRUE, outlier = TRUE)
  assignment <- attr(d, "cluster")

  ### stream has no assignments!
  if (length(assignment) == 0)
    assignment <- rep(1L, nrow(d))

  noise <- is.na(assignment)

  ### assignment is not numeric
  if (!is.numeric(assignment))
    assignment <- as.integer(as.factor(assignment))

  ### add alpha shading to color
  if (is.null(col)) {
    col <- rgb(cbind(t(col2rgb(assignment) / 255)), alpha = alpha)
  } else{
    if (length(col) == 1L)
      col <- rep(col, length(assignment))
  }

  col[noise] <-  .noise_col

  if (is.null(pch)) {
    #pch <- rep(1, n)
    pch <- as.integer(assignment)
    pch <- pch %% 25
    pch[noise] <- .noise_pch
  }

  if (!is.null(dim))
    d <- d[, dim, drop = FALSE]

  if (ncol(d) <= 2L && (method == "pairs" || method == "pca"))
    method <- "plot"

  switch(
    method,
    pairs = pairs(d, col = col, pch = pch, ...),
    pca = {
      ## we assume Euclidean here
      p <- prcomp(d)

      plot(p$x, col = col, pch = pch, ...)
      title(sub = paste(
        "Explains ",
        round(sum(p$sdev[1:2]) / sum(p$sdev) * 100, 2),
        "% of the point variability",
        sep = ""
      ))
    },
    plot = {
      if (ncol(d) > 2L)
        d <- d[, 1:2, drop = FALSE]
      if (ncol(d) == 1L)
        d <- data.frame(pos = seq_len(nrow(d)), d)
      plot(d, col = col, pch = pch, ...)
    },
    ts = {
    if (ncol(d) != 1L)
      stop("Choose a single variable containing the time series using 'dim'!")
      d <- data.frame(pos = seq_len(nrow(d)), d)
      plot(d, col = col, pch = pch, type = "l", ...)
    }
  )
}
