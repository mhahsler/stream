#' Agreement-based Measures for Clustering
#'
#' Calculates the agreement between two partitions, typically the known actual cluster labels
#' and the predicted cluster labels.
#'
#' This convenience function is an interface to [clue::cl_agreement()]. See `methods` in that man page
#' for a list of available methods. A measure typically used for clustering is the corrected Rand index
#' (also called adjusted Rand index). Numbers close to 1 indicate a very good agreement.
#'
#' @param predicted a vector with predicted cluster labels.
#' @param actual the known cluster labels (ground truth).
#' @param method the used method (see [clue::cl_agreement()]).
#' @param na_as_cluster logical; should `NA` labels (noise points) be considered its own cluster?
#' @references
#' Hornik K (2005). A CLUE for CLUster Ensembles. _Journal of Statistical Software_, *14*(12).
#' \doi{10.18637/jss.v014.i12}
#'
#' @examples
#' # Perfect agreement (1 and 2 are just switched)
#' actual <- c(2, 2, 1, 3, 2, NA)
#' predicted <- c(1, 1, 2, 3, 1, NA)
#' agreement(actual, predicted)
#'
#' # No agreement
#' predicted <- sample(predicted)
#' agreement(actual, predicted)
#' @export
agreement <- function(predicted, actual, method = "cRand", na_as_cluster = TRUE) {
  predicted <- as.integer(predicted)
  actual <- as.integer(actual)

  if (na_as_cluster) {
    predicted[is.na(predicted)] <- 0L
    actual[is.na(actual)] <- 0L
  }

  predicted <- clue::as.cl_hard_partition(predicted)
  actual <- clue::as.cl_hard_partition(actual)
  as.numeric(clue::cl_agreement(clue::cl_ensemble(predicted, actual), method =
      method))
}
