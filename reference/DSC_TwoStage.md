# TwoStage Clustering Process

Combines an **online clustering component**
([DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md))
and an **offline reclustering component**
([DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md))
into a single process.

## Usage

``` r
DSC_TwoStage(micro, macro)
```

## Arguments

- micro:

  Clustering algorithm used in the online stage
  ([DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md))

- macro:

  Clustering algorithm used for reclustering in the offline stage
  ([DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md))

## Value

An object of class `DSC_TwoStage` (subclass of
[DSC](http://michael.hahsler.net/stream/reference/DSC.md),
[DSC_Macro](http://michael.hahsler.net/stream/reference/DSC_Macro.md))
which is a named list with elements:

- `description`: a description of the clustering algorithms.

- `micro`: The [DSD](http://michael.hahsler.net/stream/reference/DSD.md)
  used for creating micro clusters in the online component.

- `macro`: The [DSD](http://michael.hahsler.net/stream/reference/DSD.md)
  for offline reclustering.

- `state`: an environment storing state information needed for
  reclustering.

with the two clusterers. The names are “

## Details

[`update()`](http://michael.hahsler.net/stream/reference/update.md) runs
the online micro-clustering stage and only when macro cluster
centers/weights are requested using
[`get_centers()`](http://michael.hahsler.net/stream/reference/DSC.md) or
[`get_weights()`](http://michael.hahsler.net/stream/reference/DSC.md),
then the offline stage reclustering is automatically performed.

Available clustering methods can be found in the See Also section below.

## See also

Other DSC_TwoStage:
[`DSC_DBSTREAM()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md),
[`DSC_DStream()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md),
[`DSC_evoStream()`](http://michael.hahsler.net/stream/reference/DSC_evoStream.md)

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
[`DSC_Static()`](http://michael.hahsler.net/stream/reference/DSC_Static.md),
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
stream <- DSD_Gaussians(k = 3, d = 2)

# Create a clustering process that uses a window for the online stage and
# k-means for the offline stage (reclustering)
win_km <- DSC_TwoStage(
  micro = DSC_Window(horizon = 100),
  macro = DSC_Kmeans(k = 3)
  )
win_km
#> Sliding window + k-Means (weighted) 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 0 
#> Number of macro-clusters: 0 

update(win_km, stream, 200)
win_km
#> Sliding window + k-Means (weighted) 
#> Class: DSC_TwoStage, DSC_Macro, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 3 
win_km$micro
#> Sliding window 
#> Class: DSC_Window, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 100 
win_km$macro
#> k-Means (weighted) 
#> Class: DSC_Kmeans, DSC_Macro, DSC_R, DSC 
#> Number of micro-clusters: 100 
#> Number of macro-clusters: 3 

plot(win_km, stream)

evaluate_static(win_km, stream, assign = "macro")
#> Evaluation results for macro-clusters.
#> Points were assigned to macro-clusters.
#> 
#>             numPoints      numMicroClusters      numMacroClusters 
#>          1.000000e+02          1.000000e+02          3.000000e+00 
#>        noisePredicted                   SSQ            silhouette 
#>          0.000000e+00          3.376204e-01          7.938251e-01 
#>       average.between        average.within          max.diameter 
#>          6.548251e-01          6.886761e-02          2.493522e-01 
#>        min.separation ave.within.cluster.ss                    g2 
#>          9.837076e-02          3.222057e-03          9.963625e-01 
#>          pearsongamma                  dunn                 dunn2 
#>          7.669822e-01          3.945053e-01          3.424728e+00 
#>               entropy              wb.ratio            numClasses 
#>          1.095559e+00          1.051695e-01          3.000000e+00 
#>           noiseActual        noisePrecision        outlierJaccard 
#>          0.000000e+00                   NaN                   NaN 
#>             precision                recall                    F1 
#>          1.000000e+00          1.000000e+00          1.000000e+00 
#>                purity             Euclidean             Manhattan 
#>          1.000000e+00          1.000000e+00          1.000000e+00 
#>                  Rand                 cRand                   NMI 
#>          1.000000e+00          1.000000e+00          1.000000e+00 
#>                    KP                 angle                  diag 
#>          1.000000e+00          1.000000e+00          1.000000e+00 
#>                    FM               Jaccard                    PS 
#>          1.000000e+00          1.000000e+00          1.000000e+00 
#>                    vi 
#>          0.000000e+00 
#> attr(,"type")
#> [1] "macro"
#> attr(,"assign")
#> [1] "macro"
```
