# Abstract Class for Micro Clusterers (Online Component)

Abstract class for all clustering methods that can operate **online**
and result in a set of micro-clusters.

## Usage

``` r
DSC_Micro(...)
```

## Arguments

- ...:

  further arguments.

## Details

Micro-clustering algorithms are data stream mining tasks
[DST](http://michael.hahsler.net/stream/reference/DST.md) which
implement the **online component of data stream clustering.** The
clustering is performed sequentially by using
[`update()`](http://michael.hahsler.net/stream/reference/update.md) to
add new points from a data stream to the clustering. The result is a set
of micro-clusters that can be retrieved using
[`get_clusters()`](http://michael.hahsler.net/stream/reference/DSD_MG.md).

Available clustering methods can be found in the See Also section below.

Many data stream clustering algorithms define both, the online and an
offline component to recluster micro-clusters into larger clusters
called macro-clusters. This is implemented here as class
[DSC_TwoStage](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md).

`DSC_Micro` cannot be instantiated.

## See also

Other DSC_Micro:
[`DSC_BICO()`](http://michael.hahsler.net/stream/reference/DSC_BICO.md),
[`DSC_BIRCH()`](http://michael.hahsler.net/stream/reference/DSC_BIRCH.md),
[`DSC_DBSTREAM()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md),
[`DSC_DStream()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md),
[`DSC_Sample()`](http://michael.hahsler.net/stream/reference/DSC_Sample.md),
[`DSC_Window()`](http://michael.hahsler.net/stream/reference/DSC_Window.md),
[`DSC_evoStream()`](http://michael.hahsler.net/stream/reference/DSC_evoStream.md)

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
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

## Examples

``` r
stream <- DSD_BarsAndGaussians(noise = .05)

# Use a DStream to create micro-clusters
dstream <- DSC_DStream(gridsize = 1, Cm = 1.5)
update(dstream, stream, 1000)
dstream
#> D-Stream 
#> Class: DSC_DStream, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 44 
#> Number of macro-clusters: 4 
nclusters(dstream)
#> [1] 44
plot(dstream, stream, main = "micro-clusters")
```
