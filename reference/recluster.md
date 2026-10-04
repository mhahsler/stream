# Re-clustering micro-clusters

Use an \***offline** macro clustering algorithm to recluster
micro-clusters into a final clusters.

## Usage

``` r
recluster(macro, micro, type = "auto", ...)

# S3 method for class 'DSC_Macro'
recluster(macro, micro, type = "auto", ...)
```

## Arguments

- macro:

  an empty
  [DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md).

- micro:

  an updated
  [DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md)
  with micro-clusters.

- type:

  controls which clustering is used from `micro`. Typically `auto`.

- ...:

  additional arguments passed on.

## Value

The object `macro` is altered in place and contains the clustering.

## Details

Takes centers and weights of the micro-clusters and applies the macro
clustering algorithm.

See
[DSC_TwoStage](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md)
for a convenient combination of micro and macro clustering.

## See also

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
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
[`read_saveDSC`](http://michael.hahsler.net/stream/reference/read_saveDSC.md)

## Author

Michael Hahsler

## Examples

``` r
set.seed(0)
### create a data stream and a micro-clustering
stream <- DSD_Gaussians(k = 3, d = 3)

### sample can be seen as a simple online clusterer where the sample points
### are the micro clusters.
sample <- DSC_Sample(k = 50)
update(sample, stream, 500)
sample
#> Reservoir sampling 
#> Class: DSC_Sample, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 50 

### recluster using k-means
kmeans <- DSC_Kmeans(k = 3)
recluster(kmeans, sample)

### plot clustering
plot(kmeans, stream, type = "both", main = "Macro-clusters (Sampling + k-means)")
```
