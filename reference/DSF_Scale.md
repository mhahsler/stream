# Scale a Data Stream

Converts an unscaled data stream into a scaled data stream.

## Usage

``` r
DSF_Scale(dsd = NULL, dim = NULL, center = TRUE, scale = TRUE, n = 100L)
```

## Arguments

- dsd:

  An object of class
  [DSD](http://michael.hahsler.net/stream/reference/DSD.md) to scale.

- dim:

  Integer indices or names of the dimensions to scale. The default is
  all dimensions.

- center, scale:

  Logical values or numeric vectors of scaling factors, each with one
  value per selected column (see
  [scale](https://rdrr.io/r/base/scale.html)).

- n:

  Number of points used to estimate centering and scaling values.

## Value

An object of class `DSF_Scale` (subclass of
[DSF](http://michael.hahsler.net/stream/reference/DSF.md) and
[DSD](http://michael.hahsler.net/stream/reference/DSD.md)).

## Details

If `center` or `scale` is logical, `DSF_Scale()` estimates the
corresponding values from `n` points in the stream using
[scale](https://rdrr.io/r/base/scale.html) in base. Estimating these
values advances the stream by `n` points.

## Deprecated

`DSD_ScaleStream` is deprecated. Use `DSF_Scale` instead.

## See also

[scale](https://rdrr.io/r/base/scale.html) in base

Other DSF:
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`DSF_Convolve()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md),
[`DSF_Downsample()`](http://michael.hahsler.net/stream/reference/DSF_Downsample.md),
[`DSF_ExponentialMA()`](http://michael.hahsler.net/stream/reference/DSF_ExponentialMA.md),
[`DSF_FeatureSelection()`](http://michael.hahsler.net/stream/reference/DSF_FeatureSelection.md),
[`DSF_Func()`](http://michael.hahsler.net/stream/reference/DSF_Func.md),
[`DSF_dplyr()`](http://michael.hahsler.net/stream/reference/DSF_dplyr.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 2)
get_points(stream, 3)
#>          X1        X2 .class
#> 1 0.1420476 0.2772531      3
#> 2 0.6543241 0.3756926      2
#> 3 0.7531830 0.3670061      2

# scale with manually calculated scaling factors
points <- get_points(stream, n = 100, info = FALSE)
center <- colMeans(points)
scale <- apply(points, MARGIN = 2, sd)

scaledStream <- stream %>% DSF_Scale(dim = c(1L, 2L), center = center, scale = scale)
colMeans(get_points(scaledStream, n = 100, info = FALSE))
#>           X1           X2 
#> -0.070876755 -0.006055181 
apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#>        X1        X2 
#> 1.0752046 0.9609059 

# let DSF_Scale calculate the scaling factors from the first n points of the stream
scaledStream <- stream %>% DSF_Scale(dim = c(1L, 2L), n = 100)
colMeans(get_points(scaledStream, n = 100, info = FALSE))
#>         X1         X2 
#> 0.07120894 0.04098528 
apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#>       X1       X2 
#> 1.059231 0.910312 

## scale only X2
scaledStream <- stream %>% DSF_Scale(dim = "X2", n = 100)
colMeans(get_points(scaledStream, n = 100, info = FALSE))
#>          X1          X2 
#>  0.37798401 -0.09605007 
apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#>        X1        X2 
#> 0.2326674 1.0229331 
```
