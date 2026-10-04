# BICO - Fast computation of k-means coresets in a data stream

Micro Clusterer. BICO maintains a tree which is inspired by the
clustering tree of BIRCH. Each node in the tree represents a subset of
these points. Instead of storing all points as individual objects, only
the number of points, the sum and the squared sum of the subset's points
are stored as key features of each subset. Points are inserted into
exactly one node.

## Usage

``` r
DSC_BICO(formula = NULL, k = 5, space = 10, p = 10, iterations = 10)
```

## Arguments

- formula:

  `NULL` to use all features in the stream or a model
  [formula](https://rdrr.io/r/stats/formula.html) of the form
  `~ X1 + X2` to specify the features used for clustering. Only `.`, `+`
  and `-` are currently supported in the formula.

- k:

  number of centers.

- space:

  coreset size.

- p:

  number of random projections used for nearest neighbor search in first
  level.

- iterations:

  number of repetitions for the kmeans++ procedure in the offline
  component.

## Details

In this implementation, the nearest neighbor search on the first level
of the tree is sped up by projecting all points to random 1-d subspaces.
The first estimation of the optimal clustering cost is computed in a
buffer phase at the beginning of the algorithm. This implementation
interfaces the original C++ implementation. For micro-clustering, the
algorithm computes the coreset of the stream. Reclustering is performed
by using the `kmeans++` algorithm on the coreset.

## References

Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris
Schwiegelshohn, Christian Sohler: BICO: BIRCH Meets Coresets for k-Means
Clustering. *ESA 2013:* 481-492.

## See also

Other DSC_Micro:
[`DSC_BIRCH()`](http://michael.hahsler.net/stream/reference/DSC_BIRCH.md),
[`DSC_DBSTREAM()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md),
[`DSC_DStream()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_Sample()`](http://michael.hahsler.net/stream/reference/DSC_Sample.md),
[`DSC_Window()`](http://michael.hahsler.net/stream/reference/DSC_Window.md),
[`DSC_evoStream()`](http://michael.hahsler.net/stream/reference/DSC_evoStream.md)

## Author

R-Interface: Matthias Carnein (<Matthias.Carnein@uni-muenster.de>),
Dennis Assenmacher. C-Implementation: Hendrik Fichtenberger, Marc Gille,
Melanie Schmidt, Chris Schwiegelshohn, Christian Sohler.

## Examples

``` r
stream <- DSD_Gaussians(k = 3, d = 2)

BICO <- DSC_BICO(k = 3, p = 10, space = 100, iterations = 10)
update(BICO, stream, n = 500)

plot(BICO,stream)
```
