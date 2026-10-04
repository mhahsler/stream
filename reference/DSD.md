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

- [`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md)

- [`print()`](https://rdrr.io/r/base/print.html)

- [`plot()`](http://michael.hahsler.net/stream/reference/plot.DSD.md)

- [`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)
  (if available)

- [`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md)
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
#> 1 0.4050989 0.44667298 0.3475992      3
#> 2 0.6763585 0.09054988 0.4570557      1
#> 3 0.4252745 0.12934062 0.7642332      2
#> 4 0.3707175 0.43533796 0.3756607      3
#> 5 0.4084845 0.11938364 0.6896738      2

# plotting the data (scatter plot matrix, first and third dimension, and first
#  two principal components)
plot(stream)

plot(stream, dim = c(1, 3))

plot(stream, method = "pca")
```
