# Agreement-based Measures for Clustering

Calculates the agreement between two partitions, typically the known
actual cluster labels and the predicted cluster labels.

## Usage

``` r
agreement(predicted, actual, method = "cRand", na_as_cluster = TRUE)
```

## Arguments

- predicted:

  a vector with predicted cluster labels.

- actual:

  the known cluster labels (ground truth).

- method:

  the used method (see
  [`clue::cl_agreement()`](https://rdrr.io/pkg/clue/man/cl_agreement.html)).

- na_as_cluster:

  logical; should `NA` labels (noise points) be considered its own
  cluster?

## Details

This convenience function is an interface to
[`clue::cl_agreement()`](https://rdrr.io/pkg/clue/man/cl_agreement.html).
See `methods` in that man page for a list of available methods. A
measure typically used for clustering is the corrected Rand index (also
called adjusted Rand index). Numbers close to 1 indicate a very good
agreement.

## References

Hornik K (2005). A CLUE for CLUster Ensembles. *Journal of Statistical
Software*, *14*(12).
[doi:10.18637/jss.v014.i12](https://doi.org/10.18637/jss.v014.i12)

## Examples

``` r
# Perfect agreement (1 and 2 are just switched)
actual <- c(2, 2, 1, 3, 2, NA)
predicted <- c(1, 1, 2, 3, 1, NA)
agreement(actual, predicted)
#> [1] 1

# No agreement
predicted <- sample(predicted)
agreement(actual, predicted)
#> [1] -0.25
```
