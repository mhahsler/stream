# Reset a Data Stream to its Beginning

Resets the position in a
[DSD](https://michael.hahsler.net/stream/reference/DSD.md) object to the
beginning or, if available, any other position in the stream.

## Usage

``` r
reset_stream(dsd, pos = 1)
```

## Arguments

- dsd:

  An object of class a subclass of
  [DSD](https://michael.hahsler.net/stream/reference/DSD.md) which
  implements a reset function.

- pos:

  Position in the stream (the beginning of the stream is position 1).

## Details

Resets the counter of the stream object. For example, for
[DSD_Memory](https://michael.hahsler.net/stream/reference/DSD_Memory.md),
the counter stored in the environment variable is moved back to 1. For
[DSD_ReadCSV](https://michael.hahsler.net/stream/reference/DSD_ReadStream.md)
objects, this is done by calling
[`seek()`](https://rdrr.io/r/base/seek.html) on the underlying
connection.

`reset_stream()` is implemented for:

- [DSD](https://michael.hahsler.net/stream/reference/DSD.md)

- [DSD_MG](https://michael.hahsler.net/stream/reference/DSD_MG.md)

- [DSD_Memory](https://michael.hahsler.net/stream/reference/DSD_Memory.md)

- [DSD_ReadStream](https://michael.hahsler.net/stream/reference/DSD_ReadStream.md)

- [DSF](https://michael.hahsler.net/stream/reference/DSF.md)

- [DSF_Convolve](https://michael.hahsler.net/stream/reference/DSF_Convolve.md)

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
[`DSD_UniformNoise()`](https://michael.hahsler.net/stream/reference/DSD_UniformNoise.md),
[`DSD_mlbenchData()`](https://michael.hahsler.net/stream/reference/DSD_mlbenchData.md),
[`DSD_mlbenchGenerator()`](https://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md),
[`DSF()`](https://michael.hahsler.net/stream/reference/DSF.md),
[`animate_data()`](https://michael.hahsler.net/stream/reference/animate_data.md),
[`close_stream()`](https://michael.hahsler.net/stream/reference/close_stream.md),
[`get_points()`](https://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](https://michael.hahsler.net/stream/reference/plot.DSD.md)

## Author

Michael Hahsler

## Examples

``` r
# initializing the objects
stream <- DSD_Gaussians()
replayer <- DSD_Memory(stream, 100)
replayer
#> Memorized Stream for Gaussian Mixture (d = 2, k = 3) 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 100 data points - currently at position 1 - loop is FALSE 

p <- get_points(replayer, 50)
replayer
#> Memorized Stream for Gaussian Mixture (d = 2, k = 3) 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 100 data points - currently at position 51 - loop is FALSE 

# reset replayer to the beginning of the stream
reset_stream(replayer)
replayer
#> Memorized Stream for Gaussian Mixture (d = 2, k = 3) 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 100 data points - currently at position 1 - loop is FALSE 

# set replayer to position 21
reset_stream(replayer, pos = 21)
replayer
#> Memorized Stream for Gaussian Mixture (d = 2, k = 3) 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 100 data points - currently at position 21 - loop is FALSE 
```
