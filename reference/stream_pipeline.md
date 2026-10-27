# Create a Data Stream Pipeline

Define a complete data stream pipe line consisting of a data stream,
filters and a data mining task using `%>%`.

## Usage

``` r
DST_Runner(dsd, dst)
```

## Arguments

- dsd:

  A data stream (subclass of
  [DSD](https://michael.hahsler.net/stream/reference/DSD.md)) typically
  provided using a `%>%` (pipe).

- dst:

  A data stream mining task (subclass of
  [DST](https://michael.hahsler.net/stream/reference/DST.md)).

## Details

A data stream pipe line consisting of a data stream, filters and a data
mining task:

`DSD %>% DSF %>% DST_Runner`

Once the pipeline is defined, it can be run using
[`update()`](https://michael.hahsler.net/stream/reference/update.md)
where points are taken from the
[DSD](https://michael.hahsler.net/stream/reference/DSD.md) data stream
source, filtered through a sequence of
[DSF](https://michael.hahsler.net/stream/reference/DSF.md) filters and
then used to update the
[DST](https://michael.hahsler.net/stream/reference/DST.md) task.

[DST_Multi](https://michael.hahsler.net/stream/reference/DST_Multi.md)
can be used to update multiple models in the pipeline with the same
stream.

## See also

Other DST:
[`DSAggregate()`](https://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](https://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](https://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](https://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](https://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST()`](https://michael.hahsler.net/stream/reference/DST.md),
[`DST_SlidingWindow()`](https://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`DST_WriteStream()`](https://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`evaluate`](https://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](https://michael.hahsler.net/stream/reference/predict.md),
[`update`](https://michael.hahsler.net/stream/reference/update.md)

## Author

Michael Hahsler

## Examples

``` r
set.seed(1500)

# Set up a pipeline with a DSD data source, DSF Filters and then a DST task
cluster_pipeline <- DSD_Gaussians(k = 3, d = 2) %>%
                    DSF_Scale() %>%
                    DST_Runner(DSC_DBSTREAM(r = .3))

cluster_pipeline
#> DST pipline runner
#> DSD: Gaussian Mixture (d = 2, k = 3)
#> + scaled
#> DST: DBSTREAM 
#> Class: DST_Runner, DST 

# the DSD and DST can be accessed directly
cluster_pipeline$dsd
#> Gaussian Mixture (d = 2, k = 3)
#> + scaled 
#> Class: DSF_Scale, DSF, DSD_R, DSD 
cluster_pipeline$dst
#> DBSTREAM 
#> Class: DSC_DBSTREAM, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 0 
#> Number of macro-clusters: 0 

# update the DST using the pipeline, by default update returns the micro clusters
update(cluster_pipeline, n = 1000)

cluster_pipeline$dst
#> DBSTREAM 
#> Class: DSC_DBSTREAM, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 33 
#> Number of macro-clusters: 3 
get_centers(cluster_pipeline$dst, type = "macro")
#>           X1         X2
#> 1  0.8247073  1.1892657
#> 2  0.3829904 -0.7893955
#> 3 -1.5545524 -0.7060603
plot(cluster_pipeline$dst)
```
