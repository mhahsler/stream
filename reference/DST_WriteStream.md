# Task to Write a Stream to a File or a Connection

Writes points from a data stream DSD object to a file or a connection.

## Usage

``` r
DST_WriteStream(file, append = FALSE, ...)

# S3 method for class 'DST_WriteStream'
close_stream(dsd, ...)
```

## Arguments

- file:

  A file name or a R connection to be written to.

- append:

  Append the data to an existing file.

- ...:

  further arguments are passed on to
  [`write_stream()`](http://michael.hahsler.net/stream/reference/write_stream.md).
  Note that `close` is always `FALSE` and cannot be specified.

- dsd:

  a `DSD_WriteStream` object with an open connection.

## Details

**Note:** `header = TRUE` is not supported for files. The header would
be added for every call for update.

## See also

Other DST:
[`DSAggregate()`](http://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](http://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](http://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](http://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST()`](http://michael.hahsler.net/stream/reference/DST.md),
[`DST_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md),
[`evaluate`](http://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](http://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](http://michael.hahsler.net/stream/reference/update.md)

## Author

Michael Hahsler

## Examples

``` r
set.seed(1500)

stream <- DSD_Gaussians(k = 3, d = 2)
writer <- DST_WriteStream(file = "data.txt", info = TRUE)

update(writer, stream, n = 2)
readLines("data.txt")
#> [1] "0.876254652435868,0.522936851845779,2"
#> [2] "0.812427512321428,0.262452294398377,1"
update(writer, stream, n = 3)
readLines("data.txt")
#> [1] "0.876254652435868,0.522936851845779,2"
#> [2] "0.812427512321428,0.262452294398377,1"
#> [3] "0.753271337087053,0.245009458201735,1"
#> [4] "0.858143359987905,0.568894154264572,2"
#> [5] "0.429813150521751,0.267543195698126,3"

# clean up
close_stream(writer)

file.remove("data.txt")
#> [1] TRUE
```
