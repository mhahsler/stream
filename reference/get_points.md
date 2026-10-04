# Get Points from a Data Stream Generator

Gets points from a
[DSD](http://michael.hahsler.net/stream/reference/DSD.md) object.

## Usage

``` r
get_points(x, ...)

# S3 method for class 'DSD'
get_points(x, n = 1L, info = TRUE, ...)

remove_info(points)
```

## Arguments

- x:

  A [DSD](http://michael.hahsler.net/stream/reference/DSD.md) object.

- ...:

  Additional parameters to pass to the `get_points()` implementations.

- n:

  integer; request up to `n` points from the stream. `n = -1` returns
  all remaining points from limited streams.

- info:

  return additional columns with information about the data point (e.g.,
  a known cluster assignment).

- points:

  a data.frame with points.

## Value

Returns a [data.frame](https://rdrr.io/r/base/data.frame.html) with (up
to) `n` rows and as many columns as `x` produces.

## Details

Each DSD object has a unique way for creating/returning data points, but
they all are called through the generic function, `get_points()`. This
is done by using the S3 class system. See the man page for the specific
[DSD](http://michael.hahsler.net/stream/reference/DSD.md) class on the
semantics for each implementation of `get_points()`.

**Additional Point Information**

Additional point information (e.g., known cluster/class assignment,
noise status) can be requested with `info = TRUE`. This information is
returned as additional columns. The column names start with `.` and are
ignored by [DST](http://michael.hahsler.net/stream/reference/DST.md)
implementations. `remove_info()` is a convenience function to remove the
information columns. Examples are

- `.id` for point IDs

- `.class` for known cluster/class labels used for plotting and
  evaluation

- `.time` a time stamp for the point (can be in seconds or an index for
  ordering)

**Resetting a Stream**

Many streams can be reset using
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md).

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
[`DSD_mlbenchGenerator()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md),
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`animate_data()`](http://michael.hahsler.net/stream/reference/animate_data.md),
[`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians()
points <- get_points(stream, n = 5)
points
#>          X1         X2 .class
#> 1 0.1577454 0.07619712      3
#> 2 0.4329033 0.18742456      2
#> 3 0.4046012 0.27845186      2
#> 4 0.4261061 0.32014826      2
#> 5 0.4187696 0.43680434      2

remove_info(points)
#>          X1         X2
#> 1 0.1577454 0.07619712
#> 2 0.4329033 0.18742456
#> 3 0.4046012 0.27845186
#> 4 0.4261061 0.32014826
#> 5 0.4187696 0.43680434
```
