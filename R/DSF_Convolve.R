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

#' Apply a Filter to a Data Stream
#'
#' Applies a filter (i.e., a convolution with a filter kernel) to a data stream.
#'
#' A filter kernel is a vector with kernel weights. A few filter are provided.
#'
#' * `filter_MA(width)` creates a moving average.
#' * `filter_diff(lag)` calculates lagged differences. Note that `na.rm = TRUE` will lead to artifacts and should not be used.
#' * `filter_Hamming(width)` creates a Hamming window.
#' * `filter_Sinc(fc, fs, width, bw)` creates a windowed-sinc filter. One of `width` (filter length) or
#'   `bw` (transition bandwidth can  be used to control the filter roll-off. The relationship is \eqn{width = 4/bw}.
#'   See Chapter 16 in Smith (1997).
#'
#' `pre` and `post` are functions that are called before and after the convolution. For example, to calculate
#' RMS, you can use `pre = pow2` and `post = sqrt`. `pow2()` is a convenience function.
#' @family DSF
#'
#' @param dsd A object of class [DSD].
#' @param dim columns to which the filter should be applied. Default is all columns.
#' @param kernel filter kernel as a numeric vector of weights.
#' @param pre,post functions to be applied before and after the convolution.
#' @param na.rm logical; should NAs be ignored?
#' @param replace logical; should the column be replaced or a column with the convolved column added?
#' @param name character; the new column will be name with the old column name + `_` + `name`.
#' @return An object of class `DSF_Convolve` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @seealso [stats::filter] provides non-streaming convolution.
#' @references
#' Steven W. Smith, The Scientist and Engineer's Guide to
#' Digital Signal Processing, California Technical Pub; 1st edition (January 1, 1997).
#' ISBN 0966017633, URL: https://www.dspguide.com/
#' @examples
#' data(presidents)
#'
#' ## Example 1: Create a data stream with three copies of president approval ratings.
#' ## We will use several convolutions.
#' stream <- data.frame(
#'     approval_orig = presidents,
#'     approval_MA = presidents,
#'     approval_diff1 = presidents,
#'     .time = time(presidents)) %>%
#'   DSD_Memory()
#'
#' plot(stream, dim = 1, n = 120, method = "ts")
#'
#' ## apply a moving average filter to dimension 1 (using the column name) and diff to dimension 3
#' filteredStream <- stream %>%
#'   DSF_Convolve(kernel = filter_MA(5), dim = "approval_orig", na.rm = TRUE) %>%
#'   DSF_Convolve(kernel = filter_diff(1), dim = 3)
#' filteredStream
#'
#' ## resetting the filtered stream also resets the original stream
#' reset_stream(filteredStream)
#' ps <- get_points(filteredStream, n = 120)
#' head(ps)
#'
#' year <- ps[[".time"]]
#' approval <- remove_info(ps)
#' matplot(year, approval, type = "l", ylim = c(-20, 100))
#' legend("topright", colnames(approval), col = 1:3, lty = 1:3, bty = "n")
#'
#' ## Example 2: Create a stream with a constant sine wave and apply
#' ## a moving average, an RMS envelope and a differences
#' stream <- DSD_Memory(data.frame(y = sin(seq(0, 2 * pi - (2 * pi / 100) ,
#'   length.out = 100))), loop = TRUE)
#' plot(stream, n = 200, method = "ts")
#'
#' filteredStream <- stream %>%
#'   DSF_Convolve(kernel = filter_MA(100), dim = 1,
#'     replace = FALSE, name = "MA") %>%
#'   DSF_Convolve(kernel = filter_MA(100), pre = pow2, post = sqrt, dim = 1,
#'     replace = FALSE, name = "RMS") %>%
#'   DSF_Convolve(kernel = filter_diff(1), dim = 1,
#'     replace = FALSE, name = "diff1")
#' filteredStream
#'
#' ps <- get_points(filteredStream, n = 500)
#' head(ps)
#'
#' matplot(ps, type = "l")
#' legend("topright", colnames(ps), col = 1:4, lty = 1:4)
#'
#' ## Note that MA and RMS use a window of length 200 and are missing at the
#' ##   beginning of the stream the window is full.
#'
#' ## Filters: look at different filters
#' filter_MA(5)
#' filter_diff(1)
#' plot(filter_Hamming(20), type = "h")
#' plot(filter_Sinc(10, 100, width = 20), type = "h")
#' @export
DSF_Convolve <-
  function(dsd = NULL,
    dim = NULL,
    kernel = NULL,
    pre = NULL,
    post = NULL,
    na.rm = FALSE,
    replace = TRUE,
    name = NULL) {
    # creating the DSD object

    if (is.null(name))
      name <- deparse1(substitute(kernel))

    l <- list(
      description = paste0(
        ifelse(!is.null(dsd), dsd$description, "DSF without a specified DSD"),
        "\n  + convolved (",
        deparse1(substitute(dim)),
        ": ",
        name,
        ")"
      ),
      dsd = dsd,
      dim = dim,
      window = DSAggregate_Window(horizon = length(kernel)),
      kernel = kernel,
      pre = pre,
      post = post,
      na.rm = na.rm,
      replace = replace,
      name = name
    )
    class(l) <-
      c("DSF_Convolve", "DSF", "DSD_R", "DSD")

    l
  }

#' @export
update.DSF_Convolve <- function(object,
  dsd = NULL,
  n = 1L,
  return = "data",
  info = TRUE,
  ...) {
  .nodots(...)
  return <- match.arg(return)

  if (is.null(dsd))
    dsd <- object$dsd
  if (is.null(dsd))
    stop("No dsd specified in ", deparse1(substitute(object)), ". Specify a dsd in update().")

  # we need to process data.frame point-by-point!
  if (!inherits(dsd, "DSD")) {
    n <- nrow(dsd)
    dsd <- DSD_Memory(dsd)
  }

  if (n == 0)
    return(get_points(dsd, n = 0L, info = info))

  for (i in seq(n)) {
    win <- update(object$window, dsd, n = 1L, return = "model")

    # dims and preallocate the space for the output data frame with n rows
    if (i == 1L) {
      dims <- get_dims(object$dim, win)

      if (object$replace) {
        extra_cols <- 0L
        dims_out <- dims
        colnames_out <- colnames(win)
      } else {
        extra_cols <- length(dims)
        dims_out <- seq(ncol(win) + 1L, length.out = extra_cols)
        colnames_out <-
          c(colnames(win), paste0(colnames(win)[dims], "_", object$name))
      }

      ps <-
        data.frame(matrix(
          NA,
          nrow = 0,
          ncol = ncol(win) + extra_cols,
          dimnames = list(NULL, colnames_out)
        ))
      ps[n,] <- NA
    }


    ## copy original values (last row in window)
    ps[i, seq(ncol(win))] <- win[nrow(win), , drop = FALSE]

    ## apply pre function
    if (!is.null(object$pre))
      win[, dims] <- object$pre(win[, dims])

    ## apply convolution
    ps[i, dims_out] <-
      sapply(
        win[, dims, drop = FALSE],
        FUN = function(p)
          mean(p * object$kernel, na.rm = object$na.rm) * length(object$kernel)
      )
  }

  ## apply post
  if (!is.null(object$post))
    ps[, dims_out] <- object$post(ps[, dims_out])

  if (!info)
    ps <- remove_info(ps)

  ps
}

#' @export
reset_stream.DSF_Convolve <- function(dsd, pos = 1) {
  ## clean window
  dsd$window$RObj$reset()

  #reset_stream(dsd$dsd, pos = pos)
  NextMethod()
}

#' @rdname DSF_Convolve
#' @param width filter width.
#' @export
filter_MA <- function(width)
  rep.int(1, width) / width

#' @rdname DSF_Convolve
#' @export
filter_Hamming <- function(width) {
  M <- width - 1
  k <- (0:M) / M
  c <- 0.54 - 0.46 * cos(2 * pi * k)
  c / sum(c)
}

#' @rdname DSF_Convolve
#' @param lag an integer indicating which time lag to use.
#' @export
filter_diff <- function(lag) {
  c <- numeric(lag + 1L)
  c[1] <- -1
  c[lag + 1] <- 1
  c
}

#' @rdname DSF_Convolve
#' @param bw transition bandwidth.
#' @param fc cutoff frequency.
#' @param fs sampling frequency.
#' @export
filter_Sinc <- function(fc, fs, width = NULL, bw = NULL)  {
  if (!is.null(bw)) {
    M <- 4 / (bw / fs)
    M <- floor(M)
    if (!M %% 2)
      M <- M + 1
  } else
    M <- width

  f <- fc / fs

  i <- 1:M
  k <- sin(2 * pi * f * (i - M / 2)) / (i - M / 2)
  k[i - M / 2 == 0] <- 2 * pi * f
  k <- k * (0.54 - 0.46 * cos(2 * pi * i / M))
  k / sum(k)
}

#' @rdname DSF_Convolve
#' @param x values to be squared.
#' @export
pow2 <- function(x)
  x ^ 2
