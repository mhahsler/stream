# Exponential Moving Average over a Data Stream

Applies an exponential moving average to components of a data stream.

## Usage

``` r
DSF_ExponentialMA(dsd = NULL, dim = NULL, alpha = 0.5)
```

## Arguments

- dsd:

  The input stream as an
  [DSD](http://michael.hahsler.net/stream/reference/DSD.md) object.

- dim:

  Columns to which the filter is applied. The default is all columns.

- alpha:

  Smoothing coefficient in \\\[0, 1\]\\. Larger values discount older
  observations faster.

## Value

An object of class `DSF_ExponentialMA` (subclass of
[DSF](http://michael.hahsler.net/stream/reference/DSF.md) and
[DSD](http://michael.hahsler.net/stream/reference/DSD.md)).

## Details

The exponential moving average is calculated by:

\\S_t = \alpha Y_t + (1 - \alpha)\\ S\_{i-1}\\

with \\S_0 = Y_0\\.

## See also

Other DSF:
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`DSF_Convolve()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md),
[`DSF_Downsample()`](http://michael.hahsler.net/stream/reference/DSF_Downsample.md),
[`DSF_FeatureSelection()`](http://michael.hahsler.net/stream/reference/DSF_FeatureSelection.md),
[`DSF_Func()`](http://michael.hahsler.net/stream/reference/DSF_Func.md),
[`DSF_Scale()`](http://michael.hahsler.net/stream/reference/DSF_Scale.md),
[`DSF_dplyr()`](http://michael.hahsler.net/stream/reference/DSF_dplyr.md)

## Author

Michael Hahsler

## Examples

``` r
# Smooth a time series
data(presidents)

stream <- data.frame(
    presidents,
    .time = time(presidents)) %>%
  DSD_Memory()

plot(stream, dim = 1, n = 120, method = "ts", main = "Original")


smoothStream <- stream %>% DSF_ExponentialMA(alpha = .7)
smoothStream
#> Memorized Stream
#> + exponential MA(0.7) 
#> Class: DSF_ExponentialMA, DSF, DSD_R, DSD 

reset_stream(smoothStream)
plot(smoothStream, dim = 1, n = 120, method = "ts", main = "With ExponentialMA(.7)")
```
