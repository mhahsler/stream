# Uniform Noise Data Stream Generator

This generator produces uniform noise in a d-dimensional unit (hyper)
cube.

## Usage

``` r
DSD_UniformNoise(d = 2, range = NULL)
```

## Arguments

- d:

  Determines the number of dimensions.

- range:

  A matrix with two columns and `d` rows giving the minimum and maximum
  for each dimension. Defaults to the range of \\\[0,1\]\\.

## Value

Returns a `DSD_UniformNoise` object.(subclass of `DSD_R`, `DSD`).

## See also

Other DSD:
[`DSD()`](https://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](https://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Benchmark()`](https://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
[`DSD_Cubes()`](https://michael.hahsler.net/stream/reference/DSD_Cubes.md),
[`DSD_Gaussians()`](https://michael.hahsler.net/stream/reference/DSD_Gaussians.md),
[`DSD_MG()`](https://michael.hahsler.net/stream/reference/DSD_MG.md),
[`DSD_Memory()`](https://michael.hahsler.net/stream/reference/DSD_Memory.md),
[`DSD_Mixture()`](https://michael.hahsler.net/stream/reference/DSD_Mixture.md),
[`DSD_NULL()`](https://michael.hahsler.net/stream/reference/DSD_NULL.md),
[`DSD_ReadDB()`](https://michael.hahsler.net/stream/reference/DSD_ReadDB.md),
[`DSD_ReadStream()`](https://michael.hahsler.net/stream/reference/DSD_ReadStream.md),
[`DSD_Target()`](https://michael.hahsler.net/stream/reference/DSD_Target.md),
[`DSD_mlbenchData()`](https://michael.hahsler.net/stream/reference/DSD_mlbenchData.md),
[`DSD_mlbenchGenerator()`](https://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md),
[`DSF()`](https://michael.hahsler.net/stream/reference/DSF.md),
[`animate_data()`](https://michael.hahsler.net/stream/reference/animate_data.md),
[`close_stream()`](https://michael.hahsler.net/stream/reference/close_stream.md),
[`get_points()`](https://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](https://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](https://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

Michael Hahsler

## Examples

``` r
# create data stream with three clusters in 2D
stream <- DSD_UniformNoise(d = 2)
get_points(stream, n = 5)
#>          X1        X2 .class
#> 1 0.5280215 0.8774594     NA
#> 2 0.4793368 0.9341587     NA
#> 3 0.6284030 0.5649711     NA
#> 4 0.6855764 0.5701182     NA
#> 5 0.2943891 0.6111523     NA
plot(stream, n = 100)


# specify a different range for each dimension
stream <- DSD_UniformNoise(d = 3,
  range = rbind(c(0, 1), c(0, 10), c(0, 5)))
plot(stream, n = 100)
```
