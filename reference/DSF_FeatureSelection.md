# Select Features for a Data Stream

Selects features from a data stream using a supplied list of features.

## Usage

``` r
DSF_FeatureSelection(dsd = NULL, features)
```

## Arguments

- dsd:

  An object of class
  [DSD](https://michael.hahsler.net/stream/reference/DSD.md).

- features:

  A character vector of feature (column) names or a numeric vector of
  feature indices. All other features are removed. Special information
  columns starting with `.` are not features.

## Value

An object of class `DSF_FeatureSelection` (subclass of
[DSF](https://michael.hahsler.net/stream/reference/DSF.md) and
[DSD](https://michael.hahsler.net/stream/reference/DSD.md)).

## See also

Other DSF:
[`DSF()`](https://michael.hahsler.net/stream/reference/DSF.md),
[`DSF_Convolve()`](https://michael.hahsler.net/stream/reference/DSF_Convolve.md),
[`DSF_Downsample()`](https://michael.hahsler.net/stream/reference/DSF_Downsample.md),
[`DSF_ExponentialMA()`](https://michael.hahsler.net/stream/reference/DSF_ExponentialMA.md),
[`DSF_Func()`](https://michael.hahsler.net/stream/reference/DSF_Func.md),
[`DSF_Scale()`](https://michael.hahsler.net/stream/reference/DSF_Scale.md),
[`DSF_dplyr()`](https://michael.hahsler.net/stream/reference/DSF_dplyr.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 3)
get_points(stream, 3)
#>          X1         X2        X3 .class
#> 1 0.1283395 1.00746980 0.3102855      3
#> 2 0.2031086 0.13624809 0.8186898      2
#> 3 0.1772421 0.05920681 0.7994196      2

stream_2features <- DSF_FeatureSelection(stream, features = c("X1", "X3"))
stream_2features
#> Gaussian Mixture (d = 3, k = 3)
#> + Feature Selection (X1, X3) 
#> Class: DSF_FeatureSelection, DSF, DSD_R, DSD 

get_points(stream_2features, n = 3)
#>          X1        X3 .class
#> 1 1.0366359 0.1879495      1
#> 2 0.1485016 0.8010295      2
#> 3 0.1616006 0.3497701      3
```
