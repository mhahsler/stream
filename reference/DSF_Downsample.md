# Downsample a Data Stream

Creates a new stream that reduces the frequency of an input stream by a
specified factor.

## Usage

``` r
DSF_Downsample(dsd = NULL, factor = 1L)
```

## Arguments

- dsd:

  The input stream as an
  [DSD](http://michael.hahsler.net/stream/reference/DSD.md) object.

- factor:

  The downsampling factor.

## Value

An object of class `DSF_Downsample` (subclass of
[DSF](http://michael.hahsler.net/stream/reference/DSF.md) and
[DSD](http://michael.hahsler.net/stream/reference/DSD.md)).

## See also

Other DSF:
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`DSF_Convolve()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md),
[`DSF_ExponentialMA()`](http://michael.hahsler.net/stream/reference/DSF_ExponentialMA.md),
[`DSF_FeatureSelection()`](http://michael.hahsler.net/stream/reference/DSF_FeatureSelection.md),
[`DSF_Func()`](http://michael.hahsler.net/stream/reference/DSF_Func.md),
[`DSF_Scale()`](http://michael.hahsler.net/stream/reference/DSF_Scale.md),
[`DSF_dplyr()`](http://michael.hahsler.net/stream/reference/DSF_dplyr.md)

## Author

Michael Hahsler

## Examples

``` r
# Simple downsampling example
stream <- DSD_Memory(data.frame(rownum = seq(100))) %>% DSF_Downsample(factor = 10)
stream
#> Memorized Stream
#> + downsampled by factor 10 
#> Class: DSF_Downsample, DSF, DSD_R, DSD 

get_points(stream, n = 2)
#>    rownum
#> 1       1
#> 11     11
get_points(stream, n = 1)
#>    rownum
#> 21     21
get_points(stream, n = 5)
#>    rownum
#> 31     31
#> 41     41
#> 51     51
#> 61     61
#> 71     71

# DSD_Memory supports getting the remaining points using n = -1
get_points(stream, n = -1)
#>    rownum
#> 81     81
#> 91     91

# Downsample a time series
data(presidents)

stream <- data.frame(
    presidents,
    .time = time(presidents)) %>%
  DSD_Memory()

plot(stream, dim = 1, n = 120, method = "ts")


# downsample by taking only every 3rd data point (quarters)
downsampledStream <- stream %>% DSF_Downsample(factor = 3)

reset_stream(downsampledStream)
plot(downsampledStream, dim = 1, n = 40, method = "ts")
```
