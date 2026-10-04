# Evaluate a Data Stream Mining Task

Generic function to calculate evaluation measures for a data stream
mining task [DST](http://michael.hahsler.net/stream/reference/DST.md) on
a data stream [DSD](http://michael.hahsler.net/stream/reference/DSD.md)
object.

## Usage

``` r
evaluate_static(object, dsd, measure, n, ...)

evaluate_stream(object, dsd, measure, n, horizon, ..., verbose = FALSE)
```

## Arguments

- object:

  The [DST](http://michael.hahsler.net/stream/reference/DST.md) object
  that the evaluation measure is being requested from.

- dsd:

  The [DSD](http://michael.hahsler.net/stream/reference/DSD.md) object
  used to create the test data.

- measure:

  Evaluation measure(s) to use. If missing then all available measures
  are returned.

- n:

  The number of data points being requested.

- ...:

  Further arguments are passed on to the specific implementation (e.g.,
  see
  [evaluate.DSC](http://michael.hahsler.net/stream/reference/evaluate.DSC.md))

- horizon:

  Evaluation is done using horizon many previous points (see detail
  section).

- verbose:

  Report progress?

## Value

`evaluate` returns an object of class `stream_eval` which is a numeric
vector of the values of the requested measures.

## Details

We define two generic evaluation functions:

- `evaluate_static()` evaluates the current
  [DST](http://michael.hahsler.net/stream/reference/DST.md) model on new
  data without updating the model.

- `evaluate_stream()` evaluates the
  [DST](http://michael.hahsler.net/stream/reference/DST.md) model using
  *prequential error estimation* (see Gama, Sebastiao and Rodrigues;
  2013). The data points in the horizon are first used to calculate the
  evaluation measure and then they are used for updating the cluster
  model. A horizon of \` means that each point is evaluated and then
  used to update the model.

The available evaluation measures depend on the task. Currently
available task to evaluate:

- [DSC](http://michael.hahsler.net/stream/reference/DSC.md) via
  [evaluate.DSC](http://michael.hahsler.net/stream/reference/evaluate.DSC.md)

## References

Joao Gama, Raquel Sebastiao, Pedro Pereira Rodrigues (2013). On
evaluating stream learning algorithms. *Machine Learning,* March 2013,
Volume 90, Issue 3, pp 317-346.

## See also

Other DST:
[`DSAggregate()`](http://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](http://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](http://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](http://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST()`](http://michael.hahsler.net/stream/reference/DST.md),
[`DST_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`DST_WriteStream()`](http://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](http://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](http://michael.hahsler.net/stream/reference/update.md)

Other evaluation:
[`animate_cluster()`](http://michael.hahsler.net/stream/reference/animate_cluster.md),
[`evaluate.DSC`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md)

## Author

Michael Hahsler
