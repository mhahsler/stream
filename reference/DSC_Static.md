# Create as Static Copy of a Clustering

This representation cannot perform clustering anymore, but it also does
not need the supporting data structures. It only stores the cluster
centers and weights.

## Usage

``` r
DSC_Static(
  x,
  type = c("auto", "micro", "macro"),
  k_largest = NULL,
  min_weight = NULL
)
```

## Arguments

- x:

  The clustering (a DSD object) to copy or a list with components
  `centers` (a data frame or matrix) and `weights` (a vector with
  cluster weights).

- type:

  which clustering to copy.

- k_largest:

  only copy the k largest (highest weight) clusters.

- min_weight:

  only copy clusters with a weight larger or equal to `min_weight`.

## Value

An object of class `DSC_Static` (sub class of
[DSC](http://michael.hahsler.net/stream/reference/DSC.md),
[DSC_R](http://michael.hahsler.net/stream/reference/DSC_R.md)). The list
also contains either
[DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md) or
[DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md)
depending on what type of clustering was copied.

## See also

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
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

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

dstream <- DSC_DStream(gridsize = 0.05)
update(dstream, stream, 500)
dstream
#> D-Stream 
#> Class: DSC_DStream, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 31 
#> Number of macro-clusters: 3 
plot(dstream, stream)


# create a static copy of the clustering
static <- DSC_Static(dstream)
static
#> Static clustering 
#> Class: DSC_Static, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 31 
plot(static, stream)


# copy only the 5 largest clusters
static2 <- DSC_Static(dstream, k_largest = 5)
static2
#> Static clustering 
#> Class: DSC_Static, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 5 
plot(static2, stream)


# copy all clusters with a weight of at least .3
static3 <- DSC_Static(dstream, min_weight = .3)
static3
#> Static clustering 
#> Class: DSC_Static, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 31 
plot(static3, stream)


# create a manual clustering
static4 <- DSC_Static(list(
             centers = data.frame(X1 = c(1, 2), X2 = c(1, 2)),
             weights = c(1, 2)),
             type = "macro")
static4
#> Static clustering 
#> Class: DSC_Static, DSC_Macro, DSC_R, DSC 
#> Number of macro-clusters: 2 
plot(static4)
```
