# Reachability Micro-Cluster Reclusterer

Macro Clusterer. Implementation of reachability clustering (based on
DBSCAN's concept of reachability) to recluster a set of micro-clusters.

## Usage

``` r
DSC_Reachability(
  formula = NULL,
  epsilon,
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

- epsilon:

  radius of the epsilon-neighborhood.

- min_weight:

  micro-clusters with a weight less than this will be ignored for
  reclustering.

- description:

  optional character string to describe the clustering method.

## Value

An object of class `DSC_Reachability`. The object contains the following
items:

- description:

  The name of the algorithm in the DSC object.

- RObj:

  The underlying R object.

## Details

Two micro-clusters are directly reachable if they are within each
other's epsilon-neighborhood (i.e., the distance between the centers is
less than epsilon). Two micro-clusters are reachable if they are
connected by a chain of pairwise directly reachable micro-clusters. All
mutually reachable micro-clusters are put in the same cluster.

Reachability uses internally
[DSC_Hierarchical](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md)
with single link.

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
[`DSC_DBSCAN()`](http://michael.hahsler.net/stream/reference/DSC_DBSCAN.md),
[`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md),
[`DSC_Hierarchical()`](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md),
[`DSC_Kmeans()`](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md)

## Author

Michael Hahsler

## Examples

``` r
#' # 3 clusters with 5% noise
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# Use a moving window for "micro-clusters and recluster with DBSCAN (macro-clusters)
cl <- DSC_TwoStage(
  micro = DSC_Window(horizon = 100),
  macro = DSC_Reachability(epsilon = .05)
)

update(cl, stream, 500)
cl
#> Sliding window + Reachability 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 9 

plot(cl, stream)
```
