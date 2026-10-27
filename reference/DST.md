# Conceptual Base Class for All Data Stream Mining Tasks

Conceptual base class for all data stream mining tasks.

## Usage

``` r
DST(...)

description(x, ...)

get_model(x, ...)
```

## Arguments

- ...:

  Further arguments.

- x:

  an object of a concrete implementation of a DST.

## Details

Base class for data stream mining tasks. Types of `DST` are

- [DSAggregate](https://michael.hahsler.net/stream/reference/DSAggregate.md)
  to aggregate data streams (e.g., with a sliding window).

- [DSC](https://michael.hahsler.net/stream/reference/DSC.md) for data
  stream clustering.

- [DSClassifier](https://michael.hahsler.net/stream/reference/DSClassifier.md)
  classification for data streams.

- [DSRegressor](https://michael.hahsler.net/stream/reference/DSRegressor.md)
  regression for data streams.

- [DSOutlier](https://michael.hahsler.net/stream/reference/DSOutlier.md)
  outlier detection for data streams.

- [DSFP](https://michael.hahsler.net/stream/reference/DSFP.md) frequent
  pattern mining for data streams.

The common interface for all DST classes consists of

- [`update()`](https://michael.hahsler.net/stream/reference/update.md)
  update the DST with data points.

- description() a string describing the DST.

- get_model() returns the DST's current model (often as a data.frame or
  a R model object).

- [`predict()`](https://michael.hahsler.net/stream/reference/predict.md)
  use the learned DST model to make predictions.

and the methods in the Methods Section below.

## See also

Other DST:
[`DSAggregate()`](https://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](https://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](https://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](https://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](https://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST_SlidingWindow()`](https://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`DST_WriteStream()`](https://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`evaluate`](https://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](https://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](https://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](https://michael.hahsler.net/stream/reference/update.md)

## Author

Michael Hahsler

## Examples

``` r
DST()
#> DST is an abstract class and cannot be instantiated!
#> Available subclasses are:
#>  DSAggregate,
#>  DSC,
#>  DSClassifier,
#>  DSD,
#>  DSF,
#>  DSFP,
#>  DSOutlier,
#>  DSRegressor
```
