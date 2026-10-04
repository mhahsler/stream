# DSD Moving Generator

Creates an evolving DSD consisting of several
[MGC](http://michael.hahsler.net/stream/reference/MGC.md)s, each
representing a moving cluster.

## Usage

``` r
DSD_MG(dimension = 2, ..., labels = NULL, description = NULL)

add_cluster(x, c, label = NULL)

get_clusters(x)

remove_cluster(x, i)

# S3 method for class 'DSD_MG'
add_cluster(x, c, label = NULL)
```

## Arguments

- dimension:

  the dimension of the DSD object

- ...:

  initial set of
  [MGC](http://michael.hahsler.net/stream/reference/MGC.md)s

- description:

  An optional string used by
  [`print()`](https://rdrr.io/r/base/print.html) to describe the data
  generator.

- x:

  A `DSD_MG` object.

- c:

  The cluster that should be added to the `DSD_MG` object.

- label, labels:

  integer representing the cluster label. `NA` represents noise. If
  labels are not specified, then each new cluster gets a new label.

- i:

  The index of the cluster that should be removed from the `DSD_MG`
  object.

## Details

This DSD is able to generate complex datasets that are able to evolve
over a period of time. Its behavior is determined by a set of
[MGC](http://michael.hahsler.net/stream/reference/MGC.md)s, each
representing a moving cluster.

## See also

[MGC](http://michael.hahsler.net/stream/reference/MGC.md) for types of
moving clusters.

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
[`DSD_Cubes()`](http://michael.hahsler.net/stream/reference/DSD_Cubes.md),
[`DSD_Gaussians()`](http://michael.hahsler.net/stream/reference/DSD_Gaussians.md),
[`DSD_Memory()`](http://michael.hahsler.net/stream/reference/DSD_Memory.md),
[`DSD_Mixture()`](http://michael.hahsler.net/stream/reference/DSD_Mixture.md),
[`DSD_NULL()`](http://michael.hahsler.net/stream/reference/DSD_NULL.md),
[`DSD_ReadDB()`](http://michael.hahsler.net/stream/reference/DSD_ReadDB.md),
[`DSD_ReadStream()`](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md),
[`DSD_Target()`](http://michael.hahsler.net/stream/reference/DSD_Target.md),
[`DSD_UniformNoise()`](http://michael.hahsler.net/stream/reference/DSD_UniformNoise.md),
[`DSD_mlbenchData()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchData.md),
[`DSD_mlbenchGenerator()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md),
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`animate_data()`](http://michael.hahsler.net/stream/reference/animate_data.md),
[`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md),
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

Matthew Bolanos

## Examples

``` r
### create an empty DSD_MG
stream <- DSD_MG(dimension = 2)
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 0 clusters in 2 dimensions. Time is 1 

### add two clusters
c1 <- MGC_Random(density = 50, center = c(50, 50), parameter = 1)
add_cluster(stream, c1)
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 1 clusters in 2 dimensions. Time is 1 

c2 <- MGC_Noise(density = 1, range = rbind(c(-20, 120), c(-20, 120)))
add_cluster(stream, c2)
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 2 clusters in 2 dimensions. Time is 1 

get_clusters(stream)
#> [[1]]
#> Random Moving Generator Cluster (MGC_Random, MGC)
#> In 2 dimensions 
#> 
#> [[2]]
#> Noise (MGC_Noise, MGC)
#> In 2 dimensions 
#> 
get_points(stream, n = 5)
#>         X1       X2 .class
#> 1 50.35411 49.89896      1
#> 2 51.41467 49.49134      1
#> 3 49.97374 49.07762      1
#> 4 49.59217 48.48719      1
#> 5 50.25441 51.56740      1
plot(stream, xlim = c(-20,120), ylim = c(-20, 120))


if (interactive()) {
animate_data(stream, n = 5000, xlim = c(-20, 120), ylim = c(-20, 120))
}

### remove cluster 1
remove_cluster(stream, 1)
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 1 clusters in 2 dimensions. Time is 10.902 

get_clusters(stream)
#> [[1]]
#> Noise (MGC_Noise, MGC)
#> In 2 dimensions 
#> 
plot(stream, xlim = c(-20, 120), ylim = c(-20, 120))


### create a more complicated cluster structure (using 2 clusters with the same
### label to form an L shape)
stream <- DSD_MG(dimension = 2,
  MGC_Static(density = 10, center = c(.5, .2),   parameter = c(.4, .2),
             shape = Shape_Block),
  MGC_Static(density = 10, center = c(.6, .5),   parameter = c(.2, .4),
             shape = Shape_Block),
  MGC_Static(density = 5,  center = c(.39, .53), parameter = c(.16, .35),
             shape = Shape_Block),
  MGC_Noise( density = 1,  range = rbind(c(0,1), c(0,1))),
  labels = c(1, 1, 2, NA)
  )
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 4 clusters in 2 dimensions. Time is 1 

plot(stream, xlim = c(0, 1), ylim = c(0, 1))


### simulate the clustering of a splitting cluster
c1 <- MGC_Linear(dimension = 2, keyframelist = list(
  keyframe(time = 1,  density = 20, center = c(0,0),   parameter = 10),
  keyframe(time = 50, density = 10, center = c(50,50), parameter = 10),
  keyframe(time = 100,density = 10, center = c(50,100),parameter = 10)
))

### Note: The second cluster appears at time = 50
c2 <- MGC_Linear(dimension = 2, keyframelist = list(
  keyframe(time = 50, density = 10, center = c(50,50), parameter = 10),
  keyframe(time = 100,density = 10, center = c(100,50),parameter = 10)
))

stream <- DSD_MG(dimension = 2, c1, c2)
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 2 clusters in 2 dimensions. Time is 1 

dbstream <- DSC_DBSTREAM(r = 20, lambda = 0.1)
if (interactive()) {
purity <- animate_cluster(dbstream, stream, n = 2500, type = "micro",
                          xlim = c(-10, 120), ylim = c(-10, 120),
                          measure = "purity", horizon = 100)
}
```
