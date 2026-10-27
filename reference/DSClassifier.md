# Abstract Class for Data Stream Classifiers

Abstract class for data stream classifiers. More implementations can be
found in package streamMOA.

## Usage

``` r
DSClassifier(...)
```

## Arguments

- ...:

  Further arguments.

## See also

Other DST:
[`DSAggregate()`](https://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](https://michael.hahsler.net/stream/reference/DSC.md),
[`DSOutlier()`](https://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](https://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST()`](https://michael.hahsler.net/stream/reference/DST.md),
[`DST_SlidingWindow()`](https://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`DST_WriteStream()`](https://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`evaluate`](https://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](https://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](https://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](https://michael.hahsler.net/stream/reference/update.md)

Other DSClassifier:
[`DSClassifier_SlidingWindow()`](https://michael.hahsler.net/stream/reference/DSClassifier_SlidingWindow.md)

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
