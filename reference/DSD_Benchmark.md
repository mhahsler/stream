# Data Stream Generator for Dynamic Data Stream Benchmarks

A data stream generator that generates several dynamic streams indented
to be benchmarks to compare data stream clustering algorithms. The
benchmarks can be used to test if a clustering algorithm can follow
moving clusters, and merging and separating clusters.

## Usage

``` r
DSD_Benchmark(i = 1)
```

## Arguments

- i:

  integer; the number of the benchmark.

## Value

Returns a [DSD](http://michael.hahsler.net/stream/reference/DSD.md)
object.

## Details

Currently available benchmarks are:

- `1`: two tight clusters moving across the data space with noise and
  intersect in the middle.

- `2`: two clusters are located in two corners of the data space. A
  third cluster moves between the two clusters forth and back.

The benchmarks are created using
[DSD_MG](http://michael.hahsler.net/stream/reference/DSD_MG.md).

## See also

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Cubes()`](http://michael.hahsler.net/stream/reference/DSD_Cubes.md),
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
stream <- DSD_Benchmark(i = 1)
get_points(stream, n = 5)
#>          X1        X2 .class
#> 1 0.1127555 0.1000563      2
#> 2 0.1179650 0.1111449      2
#> 3 0.1155370 0.9058588      1
#> 4 0.1090585 0.1057890      2
#> 5 0.1105024 0.8997594      1

if (FALSE) { # \dontrun{
stream <- DSD_Benchmark(i = 1)
animate_data(stream, n = 10000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))

stream <- DSD_Benchmark(i = 2)
animate_data(stream, n = 10000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))
} # }
```
