# Data Stream Generator for Bars and Gaussians

A data stream generator which creates the shape of two bars and two
Gaussians clusters with different density.

## Usage

``` r
DSD_BarsAndGaussians(angle = NULL, noise = 0)
```

## Arguments

- angle:

  rotation in degrees. `NULL` will produce a random rotation.

- noise:

  The amount of noise that should be added to the output.

## Value

Returns a `DSD_BarsAndGaussians` object.

## See also

[DSD](http://michael.hahsler.net/stream/reference/DSD.md)

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
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
# create data stream with three clusters in 2D
stream <- DSD_BarsAndGaussians(noise = 0.1)

get_points(stream, n = 10)
#>              x         y .class
#> 1  -1.57680878  3.952047      4
#> 2   3.10325388 -4.787931      1
#> 3   2.95586698 -2.940560      1
#> 4   1.87920308  1.398521      3
#> 5   0.06656601  2.600465      3
#> 6  -0.75723004 -4.140459      2
#> 7   1.75054455 -1.853142      1
#> 8   0.71817119  2.600374      3
#> 9  -2.56848599  3.444262      4
#> 10 -1.15812960 -2.079070      2
plot(stream)
```
