# Extract a Fixed-size Sample from a Data Stream

Micro Clusterer. Extracts a sample form a data stream using Reservoir
Sampling
([DSAggregate_Sample](http://michael.hahsler.net/stream/reference/DSAggregate_Sample.md)).
The sample is stored as a set of micro-clusters to be compatible with
other data DSC stream clustering algorithms.

## Usage

``` r
DSC_Sample(k = 100, biased = FALSE)
```

## Arguments

- k:

  the number of points to be sampled from the stream.

- biased:

  if `FALSE` then a regular (unbiased) reservoir sampling is used. If
  true then the sample is biased towards keeping more recent data points
  (see Details section).

## Value

An object of class `DSC_Sample` (subclass of
[DSC](http://michael.hahsler.net/stream/reference/DSC.md),
[DSC_R](http://michael.hahsler.net/stream/reference/DSC_R.md),
[DSC_Micro](http://michael.hahsler.net/stream/reference/DSC_Micro.md)).

## Details

If `biased = FALSE` then the reservoir sampling algorithm by McLeod and
Bellhouse (1983) is used. This sampling makes sure that each data point
has the same chance to be sampled. All sampled points will have a weight
of 1. Note that this might not be ideal for an evolving stream since
very old data points have the same chance to be in the sample as newer
points.

If `bias = TRUE` then sampling prefers newer points using the modified
reservoir sampling algorithm 2.1 by Aggarwal (2006). New points are
always added. They replace a random point in the reservoir with a
probability of reservoir size over `k`. This an exponential bias
function of `2^{-lambda}` with `lambda = 1 / k`.

## References

Vitter, J. S. (1985): Random sampling with a reservoir. *ACM
Transactions on Mathematical Software,* 11(1), 37-57.

McLeod, A.I., Bellhouse, D.R. (1983): A Convenient Algorithm for Drawing
a Simple Random Sample. *Applied Statistics,* 32(2), 182-184.

Aggarwal C. (2006) On Biased Reservoir Sampling in the Presence of
Stream Evolution. *International Conference on Very Large Databases
(VLDB'06).* 607-618.

## See also

Other DSC_Micro:
[`DSC_BICO()`](http://michael.hahsler.net/stream/reference/DSC_BICO.md),
[`DSC_BIRCH()`](http://michael.hahsler.net/stream/reference/DSC_BIRCH.md),
[`DSC_DBSTREAM()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md),
[`DSC_DStream()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_Window()`](http://michael.hahsler.net/stream/reference/DSC_Window.md),
[`DSC_evoStream()`](http://michael.hahsler.net/stream/reference/DSC_evoStream.md)

## Author

Michael Hahsler

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)

sample <- DSC_Sample(k = 20)
update(sample, stream, 500)
sample
#> Reservoir sampling 
#> Class: DSC_Sample, DSC_Micro, DSC_R, DSC 
#> Number of micro-clusters: 20 

# plot micro-clusters
plot(sample, stream)


# recluster the sample with k-means
kmeans <- DSC_Kmeans(k = 3)
recluster(kmeans, sample)
plot(kmeans, stream)


# sample from an evolving stream
stream <- DSD_Benchmark(1)
sample <- DSC_Sample(k = 20)
update(sample, stream, 1000)

plot(sample, stream)

# Note: the clusters move from left to right and the sample keeps many
# outdated points

# use a biased sample to keep more recent data points
stream <- DSD_Benchmark(1)
sample <- DSC_Sample(k = 20, biased = TRUE)
update(sample, stream, 1000)
plot(sample, stream)
```
