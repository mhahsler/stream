# Abstract Class for Data Stream Regressors

Abstract class for data stream regressors. More implementations can be
found in package streamMOA.

## Usage

``` r
DSRegressor(...)
```

## Arguments

- ...:

  Further arguments.

## See also

Other DST:
[`DSAggregate()`](http://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](http://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](http://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DST()`](http://michael.hahsler.net/stream/reference/DST.md),
[`DST_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`DST_WriteStream()`](http://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`evaluate`](http://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](http://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](http://michael.hahsler.net/stream/reference/update.md)

Other DSRegressor:
[`DSRegressor_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSRegressor_SlidingWindow.md)

## Author

Michael Hahsler

## Examples

``` r
DSClassifier()
#> DSClassifier is an abstract class and cannot be instantiated!
#> 
#> Available subclasses in ‘package:stream’ are:
#>  DSClassifier_SlidingWindow
#> 
#> To get more information in R Studio, type ‘DSClassifier_’ and hit the Tab key.
```
