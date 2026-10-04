# A Data Stream Interface for Data Stored in Memory

This class provides a data stream interface for data stored in memory as
matrix-like objects (including data frames). All or a portion of the
stored data can be replayed several times.

## Usage

``` r
DSD_Memory(
  x,
  n,
  k = NA,
  outofpoints = c("warn", "ignore", "stop"),
  loop = FALSE,
  description = NULL
)
```

## Arguments

- x:

  A matrix-like object containing the data. If `x` is a DSD object then
  a data frame for `n` data points from this DSD is created.

- n:

  Number of points used if `x` is a DSD object. If `x` is a matrix-like
  object then `n` is ignored.

- k:

  Optional: The known number of clusters in the data

- outofpoints:

  Action taken if less than `n` data points are available. The default
  is to return the available data points with a warning. Other supported
  actions are:

  - `warn`: return the available points (maybe an empty data.frame) with
    a warning.

  - `ignore`: silently return the available points.

  - `stop`: stop with an error.

- loop:

  Should the stream start over when it reaches the end?

- description:

  character string with a description.

## Value

Returns a `DSD_Memory` object (subclass of
[DSD_R](http://michael.hahsler.net/stream/reference/DSD.md),
[DSD](http://michael.hahsler.net/stream/reference/DSD.md)).

## Details

In addition to regular data.frames other matrix-like objects that
provide subsetting with the bracket operator can be used. This includes
`ffdf` (large data.frames stored on disk) from package ff and
`big.matrix` from bigmemory.

**Reading the whole stream** By using `n = -1` in
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md),
the whole stream is returned.

## See also

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
[`DSD_Cubes()`](http://michael.hahsler.net/stream/reference/DSD_Cubes.md),
[`DSD_Gaussians()`](http://michael.hahsler.net/stream/reference/DSD_Gaussians.md),
[`DSD_MG()`](http://michael.hahsler.net/stream/reference/DSD_MG.md),
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
# Example 1: store 1000 points from a stream
stream <- DSD_Gaussians(k = 3, d = 2)
replayer <- DSD_Memory(stream, k = 3, n = 1000)
replayer
#> Memorized Stream for Gaussian Mixture (d = 2, k = 3) 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 1000 data points - currently at position 1 - loop is FALSE 
plot(replayer)


# creating 2 clusterers of different algorithms
dsc1 <- DSC_DBSTREAM(r = 0.1)
dsc2 <- DSC_DStream(gridsize = 0.1, Cm = 1.5)

# clustering the same data in 2 DSC objects
reset_stream(replayer) # resetting the replayer to the first position
update(dsc1, replayer, 500)
reset_stream(replayer)
update(dsc2, replayer, 500)

# plot the resulting clusterings
reset_stream(replayer)
plot(dsc1, replayer, main = "DBSTREAM")

reset_stream(replayer)
plot(dsc2, replayer, main = "D-Stream")



# Example 2: use a data.frame to create a stream (3rd col. contains the assignment)
df <- data.frame(x = runif(100), y = runif(100),
  .class = sample(1:3, 100, replace = TRUE))

# add some outliers
out <- runif(100) > .95
df[['.outlier']] <- out
df[['.class']] <- NA
head(df)
#>           x          y .class .outlier
#> 1 0.5445857 0.18653315     NA    FALSE
#> 2 0.6220062 0.28949525     NA    FALSE
#> 3 0.1833125 0.79059693     NA    FALSE
#> 4 0.3427605 0.06894763     NA    FALSE
#> 5 0.8841958 0.90928265     NA    FALSE
#> 6 0.4359995 0.33881426     NA    FALSE

stream <- DSD_Memory(df)
stream
#> Memorized Stream 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 100 data points - currently at position 1 - loop is FALSE 

reset_stream(stream)
get_points(stream, n = 5)
#>           x          y .class .outlier
#> 1 0.5445857 0.18653315     NA    FALSE
#> 2 0.6220062 0.28949525     NA    FALSE
#> 3 0.1833125 0.79059693     NA    FALSE
#> 4 0.3427605 0.06894763     NA    FALSE
#> 5 0.8841958 0.90928265     NA    FALSE

# get the remaining points
rest <- get_points(stream, n = -1)
nrow(rest)
#> [1] 95

# plot all available points with n = -1
reset_stream(stream)
plot(stream, n = -1)
```
