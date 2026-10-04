# Mixes Data Points from Several Streams into a Single Stream

This generator mixes multiple streams given specified probabilities. The
streams have to contain the same number of dimensions.

## Usage

``` r
DSD_Mixture(..., prob = NULL)
```

## Arguments

- ...:

  [DSD](http://michael.hahsler.net/stream/reference/DSD.md) objects.

- prob:

  a numeric vector with the probability for each stream that the next
  point will be drawn from that stream.

## Value

Returns a `DSD_Mixture` object.(subclass of
[DSD_R](http://michael.hahsler.net/stream/reference/DSD.md),
[DSD](http://michael.hahsler.net/stream/reference/DSD.md)).

## See also

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
[`DSD_Cubes()`](http://michael.hahsler.net/stream/reference/DSD_Cubes.md),
[`DSD_Gaussians()`](http://michael.hahsler.net/stream/reference/DSD_Gaussians.md),
[`DSD_MG()`](http://michael.hahsler.net/stream/reference/DSD_MG.md),
[`DSD_Memory()`](http://michael.hahsler.net/stream/reference/DSD_Memory.md),
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
stream1 <- DSD_Gaussians(d = 2, k = 3)
stream2 <- DSD_UniformNoise(d = 2,  range = rbind(c(-.5, 1.5), c(-.5, 1.5)))

combinedStream <- DSD_Mixture(stream1, stream2, prob = c(.9, .1))
combinedStream
#> Stream Mixture (d = 2)
#> + Gaussian Mixture (d = 2, k = 3)
#> + Uniform Noise (d = 2) 
#> Class: DSD_Mixture, DSD_R, DSD 

get_points(combinedStream, n = 20)
#>             X1           X2 .class .stream
#> 1   0.86843259  0.331758173      2       1
#> 2   0.52844181  0.200108555      3       1
#> 3   0.61134550  0.480263491      1       1
#> 4   0.55694062  0.225185860      3       1
#> 5   0.86070285  0.302935457      2       1
#> 6   0.64302540  0.464270200      1       1
#> 7  -0.04140717 -0.008971439     NA       2
#> 8   0.89332398  0.238651591      2       1
#> 9   0.84250069 -0.191815476     NA       2
#> 10  0.64434041  0.432067246      1       1
#> 11  0.88072955  0.281491522      2       1
#> 12  0.59804592  0.516742779      1       1
#> 13  0.60960344  0.509806865      1       1
#> 14  0.89438741  0.325618947      2       1
#> 15  0.60023851  0.227878879      3       1
#> 16  0.82009664  0.368891259      2       1
#> 17  0.57290441  0.263961074      3       1
#> 18  0.93330149  0.233366927      2       1
#> 19  0.59872318  0.446890121      1       1
#> 20  0.90181017  0.308962054      2       1
plot(combinedStream, n = 200)
```
