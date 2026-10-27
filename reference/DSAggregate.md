# Data Stream Aggregator Base Classes

Abstract base classes for all DSAggregate (Data Stream Aggregator)
classes to aggregate streams. DSAggreagate is a
[DST](https://michael.hahsler.net/stream/reference/DST.md) task.

## Usage

``` r
DSAggregate(...)

# S3 method for class 'DSAggregate'
update(object, dsd, n = 1, return = c("nothing", "model"), ...)

# S3 method for class 'DSAggregate'
get_points(x, ...)

# S3 method for class 'DSAggregate'
get_weights(x, ...)
```

## Arguments

- ...:

  Further arguments.

- dsd:

  a data stream object.

- n:

  the number of data points used for the update.

- return:

  a character string indicating what update returns. The default is
  `"nothing"` and `"model"` returns the aggregated data.

- x, object:

  a concrete implementation of `DSAggregate`.

## Details

The `DSAggreagate` class cannot be instantiated, but it serve as a base
class from which other DSAggregate subclasses inherit.

Data stream operators use `update.DSAggregate()` to process new data
from the [DSD](https://michael.hahsler.net/stream/reference/DSD.md)
stream. The result of the operator can be obtained via
[`get_points()`](https://michael.hahsler.net/stream/reference/get_points.md)
and
[`get_weights()`](https://michael.hahsler.net/stream/reference/DSC.md)
(if available).

## See also

Other DST:
[`DSC()`](https://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](https://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](https://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](https://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST()`](https://michael.hahsler.net/stream/reference/DST.md),
[`DST_SlidingWindow()`](https://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`DST_WriteStream()`](https://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`evaluate`](https://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](https://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](https://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](https://michael.hahsler.net/stream/reference/update.md)

Other DSAggregate:
[`DSAggregate_Sample()`](https://michael.hahsler.net/stream/reference/DSAggregate_Sample.md),
[`DSAggregate_Window()`](https://michael.hahsler.net/stream/reference/DSAggregate_Window.md)

## Author

Michael Hahsler

## Examples

``` r
DSAggregate()
#> DSAggregate is an abstract class and cannot be instantiated!
#> 
#> Available subclasses in ‘package:stream’ are:
#>  DSAggregate_Sample,
#>  DSAggregate_Window
#> 
#> To get more information in R Studio, type ‘DSAggregate_’ and hit the Tab key.
```
