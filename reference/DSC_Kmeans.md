# Kmeans Macro-clusterer

Macro Clusterer. Class implements the k-means algorithm for reclustering
a set of micro-clusters.

## Usage

``` r
DSC_Kmeans(
  formula = NULL,
  k,
  weighted = TRUE,
  iter.max = 10,
  nstart = 10,
  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
  min_weight = NULL,
  description = NULL
)
```

## Arguments

- formula:

  `NULL` to use all features in the stream or a model
  [formula](https://rdrr.io/r/stats/formula.html) of the form
  `~ X1 + X2` to specify the features used for clustering. Only `.`, `+`
  and `-` are currently supported in the formula.

- k:

  either the number of clusters, say k, or a set of initial (distinct)
  cluster centers. If a number, a random set of (distinct) rows in x is
  chosen as the initial centers.

- weighted:

  use a weighted k-means (algorithm is ignored).

- iter.max:

  the maximum number of iterations allowed.

- nstart:

  if centers is a number, how many random sets should be chosen?

- algorithm:

  character: may be abbreviated.

- min_weight:

  micro-clusters with a weight less than this will be ignored for
  reclustering.

- description:

  optional character string to describe the clustering method.

## Value

An object of class `DSC_Kmeans` (subclass of
[DSC](http://michael.hahsler.net/stream/reference/DSC.md),
[DSC_R](http://michael.hahsler.net/stream/reference/DSC_R.md),
[DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md))

## Details

[`update()`](http://michael.hahsler.net/stream/reference/update.md) and
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)
invisibly return the assignment of the data points to clusters.

Please refer to function
[`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html) for more
details on the algorithm.

**Note** that this clustering cannot be updated iteratively and every
time it is used for (re)clustering, the old clustering is deleted.

## See also

Other DSC_Macro:
[`DSC_DBSCAN()`](http://michael.hahsler.net/stream/reference/DSC_DBSCAN.md),
[`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md),
[`DSC_Hierarchical()`](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Reachability()`](http://michael.hahsler.net/stream/reference/DSC_Reachability.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md)

## Author

Michael Hahsler

## Examples

``` r
# 3 clusters with 5% noise
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# Use a moving window for "micro-clusters and recluster with k-means (macro-clusters)
cl <- DSC_TwoStage(
  micro = DSC_Window(horizon = 100),
  macro = DSC_Kmeans(k = 3)
)

update(cl, stream, 500)
cl
#> Sliding window + k-Means (weighted) 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 3 

plot(cl, stream)
```
