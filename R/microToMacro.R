#' Translate Micro-cluster IDs to Macro-cluster IDs
#'
#' Returns the assignment of micro-cluster ids to macro-cluster ids for a `DSC_Macro`
#' object.
#'
#' @family DSC
#'
#' @param x a \code{DSC_Macro} object that also contains information about
#' micro-clusters.
#' @param micro A vector with micro-cluster ids. If `NULL` then the
#' assignments for all micro-clusters in `x` are returned.
#' @return A vector of the same length as `micro` with the macro-cluster
#' ids.
#' @author Michael Hahsler
#' @seealso \code{\link{DSC_Macro}}
#' @examples
#'
#' stream <- DSD_Gaussians(k=3, d=2, noise=0.05, p=c(.2,.4,.6))
#'
#' # recluster a micro-clusters
#' micro <- DSC_DStream(gridsize=0.05)
#' update(micro, stream, 500)
#'
#' macro <- DSC_Kmeans(k=3)
#' recluster(macro, micro)
#'
#' # translate all micro-cluster ids
#' microToMacro(macro)
#'
#' # plot some data points in gray
#' plot(stream, col="gray", cex=.5, xlim=c(0,1), ylim=c(0,1))
#' # add micro-clusters and use the macro-cluster ids as color and weights as size
#' points(get_centers(macro, type="micro"),
#'   col=microToMacro(macro),
#'   cex=get_weights(macro, type="micro", scale=c(.5,3)))
#' # add macro-cluster centers (size is weight)
#' points(get_centers(macro, type="macro"),
#'   cex = get_weights(macro, type="macro", scale=c(2,5)),
#'   pch=3,lwd=3, col=1:3)
#'
#' @export
microToMacro <- function(x, micro=NULL) UseMethod("microToMacro")

microToMacro.default <- function(x, micro=NULL) {
  stop(gettextf("microToMacro not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}
