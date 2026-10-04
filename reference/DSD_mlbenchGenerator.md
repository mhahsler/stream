# mlbench Data Stream Generator

A data stream generator class that interfaces data generators found in
package `mlbench`.

## Usage

``` r
DSD_mlbenchGenerator(method, ...)
```

## Arguments

- method:

  The name of the mlbench data generator. If missing then a list of all
  available generators is shown and returned.

- ...:

  Parameters for the mlbench data generator.

## Value

Returns a `DSD_mlbenchGenerator` object (subclass of
[DSD_R](http://michael.hahsler.net/stream/reference/DSD.md),
[DSD](http://michael.hahsler.net/stream/reference/DSD.md))

## Details

The `DSD_mlbenchGenerator` class is designed to be a wrapper class for
data created by data generators in the `mlbench` library.

Call `DSD_mlbenchGenerator` with missing method to get a list of
available methods.

## See also

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
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
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`animate_data()`](http://michael.hahsler.net/stream/reference/animate_data.md),
[`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md),
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

John Forrest

## Examples

``` r
DSD_mlbenchGenerator()
#> Available generators are:
#>  [1] "2dnormals" "cassini"   "circle"    "cuboids"   "friedman1" "friedman2"
#>  [7] "friedman3" "hypercube" "peak"      "ringnorm"  "shapes"    "simplex"  
#> [13] "smiley"    "spirals"   "threenorm" "twonorm"   "waveform"  "xor"      

stream <- DSD_mlbenchGenerator(method = "cassini")
stream
#> mlbench: cassini 
#> Class: DSD_mlbenchGenerator, DSD_R, DSD 

get_points(stream, n = 5)
#>           X1          X2 .class
#> 1 -0.4337012 -1.61497441      1
#> 2 -0.1881505  1.02670452      2
#> 3 -1.3104811  0.86853841      2
#> 4 -0.1213894 -1.17513021      1
#> 5  0.1050002 -0.07997583      3

plot(stream, n = 500)
```
