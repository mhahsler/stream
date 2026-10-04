# Abstract Class for Implementing R-based Clusterers

Abstract class for implementing R-based clusterers.

## Usage

``` r
DSC_R(...)

# S3 method for class 'DSC_R'
update(
  object,
  dsd,
  n = 1L,
  verbose = FALSE,
  block = 10000L,
  return = c("nothing", "assignment", "model"),
  ...
)
```

## Arguments

- ...:

  further arguments.

- object:

  a DSC object.

- dsd:

  a data stream object.

- n:

  number of data points taken from the stream.

- verbose:

  logical; show progress?

- block:

  process blocks of data to improve speed.

- return:

  a character string indicating what update returns. The default is
  `"nothing"`. Other possible values depend on the `DST`. Examples are
  `"model"` and `"assignment"`.

## Details

DSC_R cannot be instantiated.

**Implementing new Classes**

To implement a new clusterer you need to create an S3 class with
elements `description` and `RObj`. `RObj` needs to be a reference class
with methods:

- `cluster(newdata, ...)`

- `get_microclusters(...)`

- `get_microweights(...)`

- `get_macroclusters(...)`

- `get_macroweights(...)`

- `microToMacro(micro, ...)`

See [DSC](http://michael.hahsler.net/stream/reference/DSC.md) for
details and parameters.

`DSC_R` cannot be instantiated.

## See also

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
[`DSC_Static()`](http://michael.hahsler.net/stream/reference/DSC_Static.md),
[`DSC_TwoStage()`](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md),
[`animate_cluster()`](http://michael.hahsler.net/stream/reference/animate_cluster.md),
[`evaluate.DSC`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md),
[`get_assignment()`](http://michael.hahsler.net/stream/reference/get_assignment.md),
[`plot.DSC()`](http://michael.hahsler.net/stream/reference/plot.DSC.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`prune_clusters()`](http://michael.hahsler.net/stream/reference/prune_clusters.md),
[`read_saveDSC`](http://michael.hahsler.net/stream/reference/read_saveDSC.md),
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)

## Author

Michael Hahsler
