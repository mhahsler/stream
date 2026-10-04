# Stream Interface for Data Sets From mlbench

Provides a convenient stream interface for data sets from the mlbench
package.

## Usage

``` r
DSD_mlbenchData(data = NULL, loop = FALSE, random = FALSE, scale = FALSE)
```

## Arguments

- data:

  The name of the dataset from mlbench. If missing then a list of all
  available data sets is shown and returned.

- loop:

  logical; loop or not to loop over the data frame.

- random:

  logical; should the data be used a random order?

- scale:

  logical; apply scaling to the data?

## Value

Returns a `DSD_mlbenchData` object which is also of class
[DSD_Memory](http://michael.hahsler.net/stream/reference/DSD_Memory.md).

## Details

The `DSD_mlbenchData` class is designed to be a wrapper class for data
from the mlbench package.

All data is held in memory in either data frame or matrix form. It is
served as a stream using the
[DSD_Memory](http://michael.hahsler.net/stream/reference/DSD_Memory.md)
class. The stream can be reset to position 1 using
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md).

Call `DSD_mlbenchData` with a missing value for `data` to get a list of
all available data sets.

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
[`DSD_mlbenchGenerator()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md),
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`animate_data()`](http://michael.hahsler.net/stream/reference/animate_data.md),
[`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md),
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)

## Author

Michael Hahsler and Matthew Bolanos

## Examples

``` r
DSD_mlbenchData()
#> Available data sets:
#>  [1] "BostonHousing"       "BostonHousing2"      "BreastCancer"       
#>  [4] "DNA"                 "Glass"               "Ionosphere"         
#>  [7] "LetterRecognition"   "Ozone"               "PimaIndiansDiabetes"
#> [10] "Satellite"           "Servo"               "Shuttle"            
#> [13] "Sonar"               "Soybean"             "Vehicle"            
#> [16] "Vowel"               "Zoo"                 "HouseVotes84"       

stream <- DSD_mlbenchData("Shuttle")
stream
#> mlbench:Shuttle(d = 9, k = 7) 
#> Class: DSD_mlbenchData, DSD_Memory, DSD_R, DSD 
#> Contains 58000 data points - currently at position 1 - loop is FALSE 

get_points(stream, n = 5)
#>   V1 V2 V3 V4 V5  V6 V7 V8 V9 .class
#> 1 50 21 77  0 28   0 27 48 22      2
#> 2 55  0 92  0  0  26 36 92 56      4
#> 3 53  0 82  0 52  -5 29 30  2      1
#> 4 37  0 76  0 28  18 40 48  8      1
#> 5 37  0 79  0 34 -26 43 46  2      1

plot(stream, n = 100)
```
