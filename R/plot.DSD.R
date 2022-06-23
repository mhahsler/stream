#' Plot Data Stream Data
#'
#' Method to plot data stream data. To plot [DSC] see [plot.DSC()].
#'
#' @family DSD
#' @family plot
#'
#' @aliases plot
#'
#' @param x the [DSD] object to be plotted.
#' @param n number of plots taken from `x` to plot.
#' @param col colors used for points.
#' @param pch symbol type.
#' @param method method used for plotting: `"pairs"` (pairs plot), `"scatter"`
#' (scatter plot), `"pca"` (plot first 2 principal components), or `"ts"` (time series).
#' @param dim an integer vector with the dimensions to plot. If `NULL`
#' then for methods `pairs` and `"pca"` all dimensions are used and
#' for `"scatter"` the first two dimensions are plotted.
#' @param alpha alpha shading used to plot the points.
#' @param transform a function that maps data stream points onto a 2-D plane for plotting.
#' @param \dots further arguments are passed on to [graphics::plot.default()] or [graphics::pairs()].
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k=3, d=3)
#'
#' ## plot data
#' plot(stream, n = 500)
#' plot(stream, method = "pca", n = 500)
#' plot(stream, method = "scatter", dim = c(1, 3), n = 500)
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
#' ## plot a time series using the AirPassenger data with the total monthly
#' ## passengers from 1949 to 1960) a as a stream
#' AirPassengers
#' stream <- DSD_Memory(data.frame(
#'   .time = time(AirPassengers),
#'   passengers = AirPassengers))
#'
#' get_points(stream, n = 10)
#' plot(stream, n = 100, method = "ts")
#' @export
plot.DSD <- function(x,
  n = 500,
  col = NULL,
  pch = NULL,
  ...,
  method = c("pairs", "scatter", "pca", "ts"),
  dim = NULL,
  alpha = .6,
  transform = NULL) {
  method <- match.arg(method)

  d <- get_points(x, n, info = TRUE, outofpoints = "warn")

  assignment <- d[['.class']]
  ### stream has no assignments!
  if (is.null(assignment))
    assignment <- rep(1L, nrow(d))
  ### assignment is not numeric
  if (!is.numeric(assignment))
    assignment <- as.integer(as.factor(assignment))

  noise <- is.na(assignment)
  time <- d[['.time']]
  outlier <- d[['.outlier']]

  d <- remove_info(d)

  ### add alpha shading to color
  if (is.null(col)) {
    col <- rgb(cbind(t(col2rgb(assignment) / 255)), alpha = alpha)
  } else{
    if (length(col) == 1L)
      col <- rep(col, length(assignment))
  }

  col[noise] <-  .noise_col
  if (!is.null(outlier))
    col[outlier] <-  .outlier_col


  if (is.null(pch)) {
    #pch <- rep(1, n)
    pch <- as.integer(assignment)
    pch <- pch %% 25
    pch[noise] <- .noise_pch
    if (!is.null(outlier))
      pch[outlier] <-  .outlier_pch
  }

  if (!is.null(dim))
    d <- d[, dim, drop = FALSE]

  if (ncol(d) <= 2L && (method == "pairs" || method == "pca"))
    method <- "scatter"

  if (!is.null(transform))
    method <- "transform"

  switch(
    method,
    transform = plot(transform(d), col = col, pch = pch, ...),
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
    scatter = {
      if (ncol(d) > 2L)
        d <- d[, 1:2, drop = FALSE]
      if (ncol(d) == 1L)
        d <- data.frame(pos = seq_len(nrow(d)), d)
      plot(d, col = col, pch = pch, ...)
    },
    ts = {
      if (ncol(d) != 1L)
        stop("Choose a single variable containing the time series using 'dim'!")

      if (is.null(time))
        time <- seq_len(nrow(d))

      d <- data.frame(time = time, d)
      plot(d,
        col = col,
        pch = pch,
        type = "l",
        ...)
    }
  )
}
