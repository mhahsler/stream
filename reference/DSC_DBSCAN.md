# DBSCAN Macro-clusterer

Macro Clusterer. Implements the DBSCAN algorithm for reclustering
micro-clusterings.

## Usage

``` r
DSC_DBSCAN(
  formula = NULL,
  eps,
  MinPts = 5,
  weighted = TRUE,
  description = NULL
)
```

## Arguments

- formula:

  `NULL` to use all features in the stream or a model
  [formula](https://rdrr.io/r/stats/formula.html) of the form
  `~ X1 + X2` to specify the features used for clustering. Only `.`, `+`
  and `-` are currently supported in the formula.

- eps:

  radius of the eps-neighborhood.

- MinPts:

  minimum number of points required in the eps-neighborhood.

- weighted:

  logical indicating if a weighted version of DBSCAN should be used.

- description:

  optional character string to describe the clustering method.

## Value

An object of class `DSC_DBSCAN` (a subclass of
[DSC](http://michael.hahsler.net/stream/reference/DSC.md),
[DSC_R](http://michael.hahsler.net/stream/reference/DSC_R.md),
[DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md)).

## Details

DBSCAN is a weighted extended version of the implementation in fpc where
each micro-cluster center is considered a pseudo-point. For the MinPts
comparison, the sum of the micro-cluster weights is used instead of the
number of micro-clusters.

DBSCAN first finds core points based on the number of other points in
its eps-neighborhood. Then core points are joined into clusters using
reachability (overlapping eps-neighborhoods).

[`update()`](http://michael.hahsler.net/stream/reference/update.md) and
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)
invisibly return the assignment of the data points to clusters.

**Note** that this clustering cannot be updated iteratively and every
time it is used for (re)clustering, the old clustering is deleted.

## References

Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). A
density-based algorithm for discovering clusters in large spatial
databases with noise. In Evangelos Simoudis, Jiawei Han, Usama M.
Fayyad. *Proceedings of the Second International Conference on Knowledge
Discovery and Data Mining (KDD-96).* AAAI Press. pp. 226-231.

## See also

Other DSC_Macro:
[`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md),
[`DSC_Hierarchical()`](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md),
[`DSC_Kmeans()`](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Reachability()`](http://michael.hahsler.net/stream/reference/DSC_Reachability.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md)

## Author

Michael Hahsler

## Examples

``` r
# 3 clusters with 5% noise
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# Use a moving window for "micro-clusters and recluster with DBSCAN (macro-clusters)
cl <- DSC_TwoStage(
  micro = DSC_Window(horizon = 100),
  macro = DSC_DBSCAN(eps = .05)
)

update(cl, stream, 500)
cl
#> Sliding window + DBSCAN (weighted) 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 3 

plot(cl, stream)
```
