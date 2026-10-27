# Prune Clusters from a Clustering

Creates a (static) copy of a clustering where a fraction of the weight
or the number of clusters with the lowest weights were pruned.

## Usage

``` r
prune_clusters(dsc, threshold = 0.05, weight = TRUE)
```

## Arguments

- dsc:

  The DSC object to be pruned.

- threshold:

  The numeric vector of probabilities for the quantile.

- weight:

  should a fraction of the total weight in the clustering be pruned?
  Otherwise a fraction of clusters is pruned.

## Value

Returns an object of class `DSC_Static`.

## See also

[`DSC_Static`](https://michael.hahsler.net/stream/reference/DSC_Static.md)

Other DSC:
[`DSC()`](https://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](https://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](https://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](https://michael.hahsler.net/stream/reference/DSC_R.md),
[`DSC_SlidingWindow()`](https://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
[`DSC_Static()`](https://michael.hahsler.net/stream/reference/DSC_Static.md),
[`DSC_TwoStage()`](https://michael.hahsler.net/stream/reference/DSC_TwoStage.md),
[`animate_cluster()`](https://michael.hahsler.net/stream/reference/animate_cluster.md),
[`evaluate.DSC`](https://michael.hahsler.net/stream/reference/evaluate.DSC.md),
[`get_assignment()`](https://michael.hahsler.net/stream/reference/get_assignment.md),
[`plot.DSC()`](https://michael.hahsler.net/stream/reference/plot.DSC.md),
[`predict`](https://michael.hahsler.net/stream/reference/predict.md),
[`read_saveDSC`](https://michael.hahsler.net/stream/reference/read_saveDSC.md),
[`recluster()`](https://michael.hahsler.net/stream/reference/recluster.md)

## Author

Michael Hahsler

## Examples

``` r

# 3 clusters with 10% noise
stream <- DSD_Gaussians(k=3, noise=0.1)

dbstream <- DSC_DBSTREAM(r=0.1)
update(dbstream, stream, 500)
dbstream
#> DBSTREAM 
#> Class: DSC_DBSTREAM, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 13 
#> Number of macro-clusters: 3 
plot(dbstream, stream)


# prune lightest micro-clusters for 20% of the weight of the clustering
static <- prune_clusters(dbstream, threshold=0.2)
static
#> Static clustering 
#> Class: DSC_Static, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 6 
plot(static, stream)

```
