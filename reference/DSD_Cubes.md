# Static Cubes Data Stream Generator

A data stream generator that produces a data stream with static (hyper)
cubes filled uniformly with data points.

## Usage

``` r
DSD_Cubes(k = 2, d = 2, center, size, p, noise = 0, noise_range)
```

## Arguments

- k:

  Determines the number of clusters.

- d:

  Determines the number of dimensions.

- center:

  A matrix of means for each dimension of each cluster.

- size:

  A `k` times `d` matrix with the cube dimensions.

- p:

  A vector of probabilities that determines the likelihood of generating
  a data point from a particular cluster.

- noise:

  Noise probability between 0 and 1. Noise is uniformly distributed
  within noise range (see below).

- noise_range:

  A matrix with d rows and 2 columns. The first column contains the
  minimum values and the second column contains the maximum values for
  noise.

## Value

Returns a `DSD_Cubes` object (subclass of
[DSD_R](http://michael.hahsler.net/stream/reference/DSD.md),
[DSD](http://michael.hahsler.net/stream/reference/DSD.md)).

## See also

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
[`DSD_Gaussians()`](http://michael.hahsler.net/stream/reference/DSD_Gaussians.md),
[`DSD_MG()`](http://michael.hahsler.net/stream/reference/DSD_MG.md),
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

Michael Hahsler

## Examples

``` r
# create data stream with three clusters in 3D
stream <- DSD_Cubes(k = 3, d = 3, noise = 0.05)

get_points(stream, n = 5)
#>          X1        X2      <NA> .class
#> 1 0.3200669 0.7458459 0.3692036      2
#> 2 0.6977560 0.6218788 0.4782292      3
#> 3 0.5286707 0.5140967 0.4073528      3
#> 4 0.4748724 0.4424403 0.5262905      1
#> 5 0.9297055 0.2136115 0.3842991     NA

plot(stream)
```
