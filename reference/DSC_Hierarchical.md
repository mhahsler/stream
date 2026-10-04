# Hierarchical Micro-Cluster Reclusterer

Macro Clusterer. Implementation of hierarchical clustering to recluster
a set of micro-clusters.

## Usage

``` r
DSC_Hierarchical(
  formula = NULL,
  k = NULL,
  h = NULL,
  method = "complete",
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

  The number of desired clusters.

- h:

  Height where to cut the dendrogram.

- method:

  the agglomeration method to be used. This should be (an unambiguous
  abbreviation of) one of `"ward"`, `"single"`, `"complete"`,
  "`average"`, `"mcquitty"`, `"median"` or `"centroid"`.

- min_weight:

  micro-clusters with a weight less than this will be ignored for
  reclustering.

- description:

  optional character string to describe the clustering method.

## Value

A list of class
[DSC](http://michael.hahsler.net/stream/reference/DSC.md),
[DSC_R](http://michael.hahsler.net/stream/reference/DSC_R.md),
[DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
and `DSC_Hierarchical`. The list contains the following items:

- description:

  The name of the algorithm in the DSC object.

- RObj:

  The underlying R object.

## Details

Please refer to [`hclust()`](https://rdrr.io/r/stats/hclust.html) for
more details on the behavior of the algorithm.

[`update()`](http://michael.hahsler.net/stream/reference/update.md) and
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)
invisibly return the assignment of the data points to clusters.

**Note** that this clustering cannot be updated iteratively and every
time it is used for (re)clustering, the old clustering is deleted.

## See also

Other DSC_Macro:
[`DSC_DBSCAN()`](http://michael.hahsler.net/stream/reference/DSC_DBSCAN.md),
[`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md),
[`DSC_Kmeans()`](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Reachability()`](http://michael.hahsler.net/stream/reference/DSC_Reachability.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# Use a moving window for "micro-clusters and recluster with HC (macro-clusters)
cl <- DSC_TwoStage(
  micro = DSC_Window(horizon = 100),
  macro = DSC_Hierarchical(h = .1, method = "single")
)

update(cl, stream, 500)
cl
#> Sliding window + Hierarchical (single) 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 6 

plot(cl, stream)
```
