# Apply a dplyr Transformation to a Data Stream

Applies dplyr transformations to a data stream.

## Usage

``` r
DSF_dplyr(dsd = NULL, func = NULL, info = FALSE)
```

## Arguments

- dsd:

  An object of class
  [DSD](https://michael.hahsler.net/stream/reference/DSD.md).

- func:

  A dplyr expression.

- info:

  Logical; should the function also receive and modify the information
  columns?

## Value

An object of class `DSF_dplyr` (subclass of
[DSF](https://michael.hahsler.net/stream/reference/DSF.md) and
[DSD](https://michael.hahsler.net/stream/reference/DSD.md)).

## Details

dplyr needs to be installed and loaded with
[`library(dplyr)`](https://dplyr.tidyverse.org) before `DSF_dplyr` can
be used.

Since streams are processed one point or block at a time, only
[dplyr::dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html)
operations that work on individual rows are allowed on streams. Examples
are:

- [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

- [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

- [`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)

- [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)

- [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

Summary functions can be used, but will only be applied to the requested
part of the stream of length `n`.

`DSF_dplyr()` calls the function using `points %>% <func>` and multiple
`dplyr` functions can be applied by using `%>%` between them.

## See also

Other DSF:
[`DSF()`](https://michael.hahsler.net/stream/reference/DSF.md),
[`DSF_Convolve()`](https://michael.hahsler.net/stream/reference/DSF_Convolve.md),
[`DSF_Downsample()`](https://michael.hahsler.net/stream/reference/DSF_Downsample.md),
[`DSF_ExponentialMA()`](https://michael.hahsler.net/stream/reference/DSF_ExponentialMA.md),
[`DSF_FeatureSelection()`](https://michael.hahsler.net/stream/reference/DSF_FeatureSelection.md),
[`DSF_Func()`](https://michael.hahsler.net/stream/reference/DSF_Func.md),
[`DSF_Scale()`](https://michael.hahsler.net/stream/reference/DSF_Scale.md)

## Author

Michael Hahsler

## Examples

``` r

library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

stream <- DSD_Gaussians(k = 3, d = 3)
plot(stream, xlim = c(0, 1), ylim = c(0, 1))


# 1. Select only columns X1 and X2
# 2. filter points by X1 > .5 (Note that the info columns also need to be filtered!)
# 3. Add a sum columns

stream2 <- stream %>%
  DSF_dplyr(select(X1, X2)) %>%
  DSF_dplyr(filter(X1 > .5), info = TRUE) %>%
  DSF_dplyr(mutate(Xsum = X1 + X2))
stream2
#> Gaussian Mixture (d = 3, k = 3)
#> + function: select(X1, X2)
#> + function: filter(X1 > 0.5)
#> + function: mutate(Xsum = X1 + X2) 
#> Class: DSF_dplyr, DSF, DSD_R, DSD 

# Note: you get fewer points because of the filter operation.
get_points(stream2, n = 10)
#>          X1        X2     Xsum .class
#> 1 0.8548183 0.6106861 1.465504      1
#> 2 0.7018771 0.8458511 1.547728      2
#> 3 0.8772214 0.6283592 1.505581      1
#> 4 0.6753347 0.8272068 1.502541      2
#> 5 0.7226333 0.9060864 1.628720      2
#> 6 0.7819094 0.5098638 1.291773      1
plot(stream2, xlim = c(0, 1), ylim = c(0, 1))
```
