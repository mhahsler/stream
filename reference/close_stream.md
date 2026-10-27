# Close a Data Stream

Close a data stream that needs closing (e.g., a file or a connection).

## Usage

``` r
close_stream(dsd, ...)
```

## Arguments

- dsd:

  An object of class a subclass of
  [DSD](https://michael.hahsler.net/stream/reference/DSD.md) which
  implements a reset function.

- ...:

  further arguments.

## Details

`close_stream()` is implemented for:

- [DSD](https://michael.hahsler.net/stream/reference/DSD.md)

- [DSD_ReadCSV](https://michael.hahsler.net/stream/reference/DSD_ReadStream.md)

- [DSD_ReadDB](https://michael.hahsler.net/stream/reference/DSD_ReadDB.md)

- [DSD_ReadStream](https://michael.hahsler.net/stream/reference/DSD_ReadStream.md)

- [DSF](https://michael.hahsler.net/stream/reference/DSF.md)

- [DST_WriteStream](https://michael.hahsler.net/stream/reference/DST_WriteStream.md)

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
[`get_points()`](https://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](https://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](https://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

Michael Hahsler
