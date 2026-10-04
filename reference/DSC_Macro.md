# Abstract Class for Macro Clusterers (Offline Component)

Abstract class for all DSC Macro Clusterers which recluster
micro-clusters **offline** into final clusters called macro-clusters.

## Usage

``` r
DSC_Macro(...)

microToMacro(x, micro = NULL)
```

## Arguments

- ...:

  further arguments.

- x:

  a `DSC_Macro` object that also contains information about
  micro-clusters.

- micro:

  A vector with micro-cluster ids. If `NULL` then the assignments for
  all micro-clusters in `x` are returned.

## Value

A vector of the same length as `micro` with the macro-cluster ids.

## Details

Data stream clustering algorithms typically consist of an **online
component** that creates micro-clusters (implemented as
[DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md))
and **offline components** which are used to recluster micro-clusters
into final clusters called macro-clusters. The function
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)
is used to extract micro-clusters from a
[DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md)
and create macro-clusters with a `DSC_Macro`.

Available clustering methods can be found in the See Also section below.

`microToMacro()` returns the assignment of Micro-cluster IDs to
Macro-cluster IDs.

For convenience, a
[DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md)
and `DSC_Macro` can be combined using
[DSC_TwoStage](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md).

`DSC_Macro` cannot be instantiated.

## See also

Other DSC_Macro:
[`DSC_DBSCAN()`](http://michael.hahsler.net/stream/reference/DSC_DBSCAN.md),
[`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md),
[`DSC_Hierarchical()`](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md),
[`DSC_Kmeans()`](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md),
[`DSC_Reachability()`](http://michael.hahsler.net/stream/reference/DSC_Reachability.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md)

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
[`DSC_Static()`](http://michael.hahsler.net/stream/reference/DSC_Static.md),
[`DSC_TwoStage()`](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md),
[`animate_cluster()`](http://michael.hahsler.net/stream/reference/animate_cluster.md),
[`evaluate.DSC`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md),
[`get_assignment()`](http://michael.hahsler.net/stream/reference/get_assignment.md),
[`plot.DSC()`](http://michael.hahsler.net/stream/reference/plot.DSC.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`prune_clusters()`](http://michael.hahsler.net/stream/reference/prune_clusters.md),
[`read_saveDSC`](http://michael.hahsler.net/stream/reference/read_saveDSC.md),
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)

## Author

Michael Hahsler
