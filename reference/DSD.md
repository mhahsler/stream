# Data Stream Data Generator Base Classes

Abstract base classes for DSD (Data Stream Data Generator).

## Usage

``` r
DSD(...)

DSD_R(...)
```

## Arguments

- ...:

  further arguments.

## Details

The `DSD` class cannot be instantiated, but it serves as a abstract base
class from which all DSD objects inherit. Implementations can be found
in the See Also section below.

`DSD` provides common functionality like:

- [`get_points()`](https://michael.hahsler.net/stream/reference/get_points.md)

- [`print()`](https://rdrr.io/r/base/print.html)

- [`plot()`](https://michael.hahsler.net/stream/reference/plot.DSD.md)

- [`reset_stream()`](https://michael.hahsler.net/stream/reference/reset_stream.md)
  (if available)

- [`close_stream()`](https://michael.hahsler.net/stream/reference/close_stream.md)
  (if needed)

`DSD_R` inherits form `DSD` and is the abstract parent class for DSD
implemented in R. To create a new R-based implementation there are only
two function that needs to be implemented for a new `DSD` subclass
called `Foo` would be:

1.  A creator function `DSD_Foo(...)` and

2.  a method `get_points.DSD_Foo(x, n = 1L)` for that class.

For details see [`vignette()`](https://rdrr.io/r/utils/vignette.html)

## See also

Other DSD:
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
[`DSD_UniformNoise()`](https://michael.hahsler.net/stream/reference/DSD_UniformNoise.md),
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
DSD()
#> DSD is an abstract class and cannot be instantiated!
#> 
#> Available subclasses in ‘package:stream’ are:
#>  DSD_BarsAndGaussians,
#>  DSD_Benchmark,
#>  DSD_Cubes,
#>  DSD_Gaussians,
#>  DSD_MG,
#>  DSD_Memory,
#>  DSD_Mixture,
#>  DSD_NULL,
#>  DSD_R,
#>  DSD_ReadCSV,
#>  DSD_ReadDB,
#>  DSD_ReadStream,
#>  DSD_ScaleStream,
#>  DSD_Target,
#>  DSD_UniformNoise,
#>  DSD_mlbenchData,
#>  DSD_mlbenchGenerator
#> 
#> To get more information in R Studio, type ‘DSD_’ and hit the Tab key.

# create data stream with three clusters in 3-dimensional space
stream <- DSD_Gaussians(k = 3, d = 3)

# get points from stream
get_points(stream, n = 5)
#>          X1         X2        X3 .class
#> 1 0.3831682 0.44882922 0.3644049      3
#> 2 0.6412330 0.02733078 0.4092730      1
#> 3 0.4461315 0.14489404 0.7313965      2
#> 4 0.4173832 0.43074978 0.3399003      3
#> 5 0.3831580 0.10049728 0.7295471      2

# plotting the data (scatter plot matrix, first and third dimension, and first
#  two principal components)
plot(stream)

plot(stream, dim = c(1, 3))

plot(stream, method = "pca")
```
