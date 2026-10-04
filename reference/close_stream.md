# Close a Data Stream

Close a data stream that needs closing (e.g., a file or a connection).

## Usage

``` r
close_stream(dsd, ...)
```

## Arguments

- dsd:

  An object of class a subclass of
  [DSD](http://michael.hahsler.net/stream/reference/DSD.md) which
  implements a reset function.

- ...:

  further arguments.

## Details

`close_stream()` is implemented for:

- [DSD](http://michael.hahsler.net/stream/reference/DSD.md)

- [DSD_ReadCSV](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md)

- [DSD_ReadDB](http://michael.hahsler.net/stream/reference/DSD_ReadDB.md)

- [DSD_ReadStream](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md)

- [DSF](http://michael.hahsler.net/stream/reference/DSF.md)

- [DST_WriteStream](http://michael.hahsler.net/stream/reference/DST_WriteStream.md)

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
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

Michael Hahsler
