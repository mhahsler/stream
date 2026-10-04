# Update a Data Stream Mining Task Model with Points from a Stream

`update()` for data stream mining tasks
[DST](http://michael.hahsler.net/stream/reference/DST.md).

## Usage

``` r
# S3 method for class 'DST'
update(object, dsd, n = 1L, return = "nothing", ...)
```

## Arguments

- object:

  The [DST](http://michael.hahsler.net/stream/reference/DST.md) object.

- dsd:

  A [DSD](http://michael.hahsler.net/stream/reference/DSD.md) object
  with the data stream.

- n:

  number of points from `dsd` to use for the update. Some DSD `dsd`
  accept `n = -1` to update with all remaining points in the stream.

- return:

  a character string indicating what update returns. The default is
  `"nothing"`. Other possible values depend on the `DST`. Examples are
  `"data"`, `"model"` and `"assignment"`.

- ...:

  Additional arguments are passed on.

## Value

`NULL` or a data.frame `n` rows containing update information for each
data point.

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
[`evaluate`](http://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](http://michael.hahsler.net/stream/reference/stream_pipeline.md)

## Author

Michael Hahsler

## Examples

``` r
set.seed(1500)
stream <- DSD_Gaussians(k = 3, d = 2, noise = .1)

dbstream <- DSC_DBSTREAM(r = .1)
assignment <- update(dbstream, stream, n = 100, return = "assignment")
plot(dbstream, stream, type = "both")


# DBSTREAM returns cluster assignments (see DSC_DBSTREAM).
head(assignment)
#>   .class .mc_id
#> 1      1      1
#> 2      2      2
#> 3      2      2
#> 4      3      3
#> 5      4      4
#> 6      5      5
```
