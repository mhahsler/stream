# Apply a Function to Transform a Data Stream

Applies an R function to transform a data stream.

## Usage

``` r
DSF_Func(dsd = NULL, func, ..., info = FALSE)
```

## Arguments

- dsd:

  An object of class
  [DSD](https://michael.hahsler.net/stream/reference/DSD.md).

- func:

  A function that takes a data frame as its first argument and returns
  the transformed data frame.

- ...:

  Further arguments passed to `func`.

- info:

  Logical; should the function also receive and modify the information
  columns?

## Value

An object of class `DSF_Func` (subclass of
[DSF](https://michael.hahsler.net/stream/reference/DSF.md) and
[DSD](https://michael.hahsler.net/stream/reference/DSD.md)).

## Details

The function's first argument needs to be a data.frame representing
points of the data stream. The function will be called as
`ps %>% your_function()`, where `ps` is the data.frame with some points
obtained using
[`get_points()`](https://michael.hahsler.net/stream/reference/get_points.md)
on the data stream source.

## See also

Other DSF:
[`DSF()`](https://michael.hahsler.net/stream/reference/DSF.md),
[`DSF_Convolve()`](https://michael.hahsler.net/stream/reference/DSF_Convolve.md),
[`DSF_Downsample()`](https://michael.hahsler.net/stream/reference/DSF_Downsample.md),
[`DSF_ExponentialMA()`](https://michael.hahsler.net/stream/reference/DSF_ExponentialMA.md),
[`DSF_FeatureSelection()`](https://michael.hahsler.net/stream/reference/DSF_FeatureSelection.md),
[`DSF_Scale()`](https://michael.hahsler.net/stream/reference/DSF_Scale.md),
[`DSF_dplyr()`](https://michael.hahsler.net/stream/reference/DSF_dplyr.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 3)
get_points(stream, n = 5)
#>           X1        X2        X3 .class
#> 1 0.45644198 0.3192106 0.7213576      2
#> 2 0.11084322 0.7060063 0.3254849      1
#> 3 0.15460289 0.6886859 0.3652530      1
#> 4 0.47480522 0.3168706 0.7498813      2
#> 5 0.08529232 0.7212409 0.3127524      1

## Example 1: rename the columns
rename <- function(x, names) {
  colnames(x) <-  names
  x
}

# By default, the info columns starting with . are not affected.
stream2 <- stream %>% DSF_Func(rename, names = c("A", "B", "C"))
stream2
#> Gaussian Mixture (d = 3, k = 3)
#> + function: rename 
#> Class: DSF_Func, DSF, DSD_R, DSD 
get_points(stream2, n = 5)
#>           A         B         C .class
#> 1 0.4342038 0.3322064 0.7157336      2
#> 2 0.3824367 0.1147064 0.1412927      3
#> 3 0.3733803 0.1483660 0.1584031      3
#> 4 0.1380883 0.7551047 0.4124708      1
#> 5 0.3474889 0.1122304 0.1654209      3

## Example 2: add a sum columns
stream3 <- stream2 %>% DSF_Func(function(x) {
  x$sum = rowSums(x)
  x
})
stream3
#> Gaussian Mixture (d = 3, k = 3)
#> + function: rename
#> + function: function(x) {; x$sum = rowSums(x); x; } 
#> Class: DSF_Func, DSF, DSD_R, DSD 
get_points(stream3, n = 5)
#>            A         B         C       sum .class
#> 1 0.07319499 0.7742238 0.3360901 1.1835089      1
#> 2 0.39464178 0.3911129 0.6904716 1.4762263      2
#> 3 0.09524489 0.7168854 0.3232897 1.1354199      1
#> 4 0.44223147 0.3676431 0.6463945 1.4562691      2
#> 5 0.45132830 0.1428343 0.1689007 0.7630633      3

## Example 3: Project the stream on its first 2 PCs (using a sample)
pr <- princomp(get_points(stream, n = 100, info = FALSE))
pca_trans <- function(x) predict(pr, x[, c("X1", "X2", "X3")])[, 1:2 , drop = FALSE]
pca_trans(get_points(stream, n = 3, info = FALSE))
#>          Comp.1     Comp.2
#> [1,]  0.3402103 -0.1265072
#> [2,] -0.1041374  0.2394933
#> [3,]  0.3966679 -0.1333895

stream4 <- stream %>% DSF_Func(pca_trans)
stream4
#> Gaussian Mixture (d = 3, k = 3)
#> + function: pca_trans 
#> Class: DSF_Func, DSF, DSD_R, DSD 

get_points(stream4, n = 3)
#>        Comp.1     Comp.2 .class
#> 1 -0.02680910  0.3166875      2
#> 2 -0.01125253  0.2773255      2
#> 3 -0.29376430 -0.1950964      1
plot(stream4)


## Example 4: Change a class labels using info = TRUE. We redefine class 3 as noise (NA)
stream5 <- stream %>% DSF_Func(
  function(x) { x[['.class']][x[['.class']] == 3] <- NA; x },
  info = TRUE)
stream5
#> Gaussian Mixture (d = 3, k = 3)
#> + function: function(x) {; x[[".class"]][x[[".class"]] == 3] <- NA; x;
#> } 
#> Class: DSF_Func, DSF, DSD_R, DSD 

get_points(stream5, n = 5)
#>          X1        X2        X3 .class
#> 1 0.4590594 0.3239982 0.7254512      2
#> 2 0.4553169 0.3295803 0.7492146      2
#> 3 0.2780631 0.1934227 0.2678634     NA
#> 4 0.3986266 0.1221774 0.1386862     NA
#> 5 0.1461974 0.7123155 0.3797266      1
plot(stream5)
```
