# Save and Read DSC Objects

Save and Read DSC objects safely (serializes the underlying data
structure). This also works for streamMOA DSC objects.

## Usage

``` r
saveDSC(object, file, ...)

readDSC(file)
```

## Arguments

- object:

  a DSC object.

- file:

  filename.

- ...:

  further arguments.

## See also

[`saveRDS`](https://rdrr.io/r/base/readRDS.html) and
[`readRDS`](https://rdrr.io/r/base/readRDS.html).

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
[`prune_clusters()`](https://michael.hahsler.net/stream/reference/prune_clusters.md),
[`recluster()`](https://michael.hahsler.net/stream/reference/recluster.md)

## Author

Michael Hahsler

## Examples

``` r

stream <- DSD_Gaussians(k = 3, noise = 0.05)

# create clusterer with r = 0.05
dbstream1 <- DSC_DBSTREAM(r = .05)
update(dbstream1, stream, 1000)
dbstream1
#> DBSTREAM 
#> Class: DSC_DBSTREAM, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 36 
#> Number of macro-clusters: 2 

saveDSC(dbstream1, file="dbstream.Rds")

dbstream2 <- readDSC("dbstream.Rds")
dbstream2
#> DBSTREAM 
#> Class: DSC_DBSTREAM, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 36 
#> Number of macro-clusters: 2 

## cleanup
unlink("dbstream.Rds")
```
