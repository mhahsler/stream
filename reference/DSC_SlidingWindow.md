# DSC_SlidingWindow – Data Stream Clusterer Using a Sliding Window

The clusterer keeps a sliding window for the stream and rebuilds a DSC
clustering model at regular intervals. By default is uses
[DSC_Kmeans](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md).
Other
[DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md)
clusterer can be used.

## Usage

``` r
DSC_SlidingWindow(formula = NULL, model = DSC_Kmeans, window, rebuild, ...)
```

## Arguments

- formula:

  a formula for the classification problem.

- model:

  regression model (that has a formula interface).

- window:

  size of the sliding window.

- rebuild:

  interval (number of points) for rebuilding the regression. Set rebuild
  to `Inf` to prevent automatic rebuilding. Rebuilding can be initiated
  manually when calling
  [`update()`](http://michael.hahsler.net/stream/reference/update.md).

- ...:

  additional parameters are passed on to the clusterer (default is
  [DSC_Kmeans](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md)).

## Value

An object of class `DST_SlidingWindow`.

## Details

This constructor creates a clusterer based on
[`DST_SlidingWindow`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md).
The clusterer has a
[`update()`](http://michael.hahsler.net/stream/reference/update.md) and
[`predict()`](http://michael.hahsler.net/stream/reference/predict.md)
method.

The difference to setting up a
[DSC_TwoStage](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md)
is that `DSC_SlidingWindow` rebuilds the model in regular intervals,
while `DSC_TwoStage` rebuilds the model on demand.

## See also

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md),
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

Other DSC_Macro:
[`DSC_DBSCAN()`](http://michael.hahsler.net/stream/reference/DSC_DBSCAN.md),
[`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md),
[`DSC_Hierarchical()`](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md),
[`DSC_Kmeans()`](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Reachability()`](http://michael.hahsler.net/stream/reference/DSC_Reachability.md)

## Author

Michael Hahsler

## Examples

``` r
library(stream)

stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

# define the stream clusterer.
cl <- DSC_SlidingWindow(
  formula = ~ . - `.class`,
  k = 3,
  window = 50,
  rebuild = 10
  )
cl
#> Data Stream Clusterer on a Sliding Window
#> Function: DSC_Kmeans 
#> Class: DSC_SlidingWindow, DST_SlidingWindow, DST 

# update the clusterer with 100 points from the stream
update(cl, stream, 100)
#> Warning: 'varlist' has changed (from nvar=2) to new 3 after EncodeVars() -- should no longer happen!

# get the cluster model
cl$model$result
#> k-Means (weighted) 
#> Class: DSC_Kmeans, DSC_Macro, DSC_R, DSC 
#> Number of micro-clusters: 50 
#> Number of macro-clusters: 3 

plot(cl$model$result)
```
