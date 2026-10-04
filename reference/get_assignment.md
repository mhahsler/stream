# Assignment Data Points to Clusters [deprecated](https://rdrr.io/r/base/Deprecated.html)

**Deprecation Notice:** use
[`predict()`](http://michael.hahsler.net/stream/reference/predict.md)
for a more general interface to apply a data stream model to new data.
`get_assignment()` is deprecated.

## Usage

``` r
get_assignment(
  dsc,
  points,
  type = c("auto", "micro", "macro"),
  method = "auto",
  ...
)

# S3 method for class 'DSC'
get_assignment(
  dsc,
  points,
  type = c("auto", "micro", "macro"),
  method = c("auto", "nn", "model"),
  ...
)
```

## Arguments

- dsc:

  The [DSC](http://michael.hahsler.net/stream/reference/DSC.md) object
  with the clusters for assignment.

- points:

  The points to be assigned as a data.frame.

- type:

  Use micro- or macro-clusters in
  [DSC](http://michael.hahsler.net/stream/reference/DSC.md) for
  assignment.

- method:

  assignment method

  - `"model"` uses the assignment method of the underlying algorithm
    (unassigned points return `NA`). Not all algorithms implement this
    option.

  - `"nn"` performs nearest neighbor assignment using Euclidean
    distance.

  - `"auto"` uses the model assignment method. If this method is not
    implemented/available then method `"nn"` is used instead.

- ...:

  Additional arguments are passed on.

## Value

A vector containing the assignment of each point. `NA` means that a data
point was not assigned to a cluster.

## Details

Get the assignment of data points to clusters in a `DSC` using the
model's assignment rules or nearest neighbor assignment. The clustering
is not modified.

Each data point is assigned either using the original model's assignment
rule or Euclidean nearest neighbor assignment. If the user specifies the
model's assignment strategy, but is not available, then nearest neighbor
assignment is used and a warning is produced.

## See also

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
[`DSC_Static()`](http://michael.hahsler.net/stream/reference/DSC_Static.md),
[`DSC_TwoStage()`](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md),
[`animate_cluster()`](http://michael.hahsler.net/stream/reference/animate_cluster.md),
[`evaluate.DSC`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md),
[`plot.DSC()`](http://michael.hahsler.net/stream/reference/plot.DSC.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`prune_clusters()`](http://michael.hahsler.net/stream/reference/prune_clusters.md),
[`read_saveDSC`](http://michael.hahsler.net/stream/reference/read_saveDSC.md),
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)

dbstream <- DSC_DBSTREAM(r = .1)
update(dbstream, stream, n = 100)

# find the assignment for the next 100 points to
# micro-clusters in dsc. This uses the model's assignment function
points <- get_points(stream, n = 100)
a <- predict(dbstream, points)
head(a)
#>   .class
#> 1      2
#> 2      3
#> 3      4
#> 4      1
#> 5      1
#> 6      1

# show the MC assignment areas. Assigned points as blue circles and
# the unassigned points as red dots
plot(dbstream, stream, assignment = TRUE, type = "none")
points(points[!is.na(a[, ".class"]),], col = "blue")
points(points[is.na(a[, ".class"]),], col = "red", pch = 20)


# use nearest neighbor assignment instead
a <- predict(dbstream, points, method = "nn")
head(a)
#>   .class
#> 1      2
#> 2      3
#> 3      4
#> 4      1
#> 5      1
#> 6      1
```
