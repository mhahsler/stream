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
#' @param transform a function that maps data stream points onto a 2-D plane for plotting.
#' @param \dots further arguments are passed on to [graphics::plot.default()] or [graphics::pairs()].
#' \pkg{graphics}.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 3, noise = 0.05)
#'
#' ## Example 1: Plot data
#' plot(stream)
#'
#' ## Example 2: Plot a clustering
#' dstream <- DSC_DStream(gridsize = 0.1)
#' update(dstream, stream, 500)
#' dstream
#' plot(dstream, stream)
#'
#' ## plot micro or macro-clusters only
#' plot(dstream, stream, type = "micro")
#' plot(dstream, stream, type = "macro")
#'
#' ## plot projected on the first two principal components
#' ## and on dimensions 2 and 3
#' plot(dstream, stream, method = "pca")
#' plot(dstream, stream, dim = c(2, 3))
#'
#' ## D-Stream has a special implementation to show assignment areas
#' plot(dstream, stream, assignment = TRUE)
#'
#' ## Example 4: Use a custom transformation for plotting.
#' ##     We fit PCA using 100 points and create a transformation
#' ##     function to project the stream to the first two PCs.
#' pr <- princomp(get_points(stream, n = 100, info = FALSE))
#' trans <- function(x) predict(pr, x)[, 1:2 , drop = FALSE]
#'
#' trans(get_points(stream, n = 3))
#'
#' plot(dstream, stream, transform = trans)
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
  type = c("auto", "micro", "macro", "both", "none"),
  assignment = FALSE,
  transform = NULL,
  ### assignment is not implemented
  ...) {
  method <- match.arg(method)
  type <- match.arg(type)

  if (is.null(col_points))
    col_points <- .points_col

  micros <- NULL
  macros <- NULL
  k_micros <- 0L
  k_macros <- 0L
  if (type == "micro" || type == "both" || type == "auto") {
    try(micros <- get_centers(x, type = "micro"), silent = TRUE)
    if (is.null(micros) && type != "auto")
      stop("No micro-clusters available!")
    if (!is.null(micros))
      k_micros <- nrow(micros)
  }
  if (type == "macro" || type == "both"|| type == "auto"){
    try(macros <- get_centers(x, type = "macro"), silent = TRUE)
    if (is.null(macros) && type != "auto")
      stop("No macro-clusters available!")
    if (!is.null(macros))
      k_macros <- nrow(macros)
  }

  # if (k_micros < 1L) {
  #   warning("No clusters to plot!")
  #   plot(NA, NA, xlim = c(0, 0), ylim = c(0, 0))
  #   return()
  # }


  # some clusterers may return an empty data.frame
  if(k_micros == 0L)
    micros <- NULL
  if(k_macros == 0L)
    macros <- NULL

  ### fix names if necessary
  if(!is.null(macros) && !is.null(micros))
    colnames(micros) <- colnames(macros)

  centers <- rbind(micros, macros)

  if (weights)
    cex_clusters <- c(
      if (k_micros > 0L)
        get_weights(x, type = "micro", scale = scale),
      if (k_macros > 0L)
        get_weights(x, type = "macro", scale = scale * 1.5)
    )
  else
    cex_clusters <-
    c(rep(cex, k_micros), rep(cex * 2, k_macros))

  col <-
    c(rep(col_clusters[1], k_micros),
      rep(col_clusters[2], k_macros))
  mpch <- c(rep(.micro_pch, k_micros), rep(.macro_pch, k_macros))
  lwd <- c(rep(1, k_micros), rep(2, k_macros))

  ### prepend data if given so it is in the background
  if (!is.null(dsd)) {
    if(inherits(dsd, "DSD"))
      d <- get_points(dsd, n, info = FALSE)
    else {
      d <- as.data.frame(dsd)
      n <- nrow(d)
    }

    if(!is.null(centers)) {
      colnames(centers) <- colnames(d)
      centers <- rbind(d, centers)
    } else
      centers <- d

    if (length(col_points) == 1L)
      col_points <- rep(col_points[1], n)
    if (length(col_points) != n)
      stop("'col_points' needs to be a single color or a vector with a color for each data point.")

    col <- c(col_points, col)
    cex_clusters <- c(rep(cex[1], n), cex_clusters)

    ## TODO: we could use cluster labels for pch
    mpch <- c(rep(1L, n), mpch)
    mpch <- mpch %% 25   ### pch cannot be more than 25

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
    centers <- centers[, dim, drop = FALSE]

  ### plot
  if (!is.null(transform)) {
    plot(
      transform(centers),
      col = col,
      cex = cex_clusters,
      pch = mpch,
      lwd = lwd,
      ...
    )
  }
  else if (method == "pairs" &&  ncol(centers) > 2L) {
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
