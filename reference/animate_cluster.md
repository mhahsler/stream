# Animates Plots of the Clustering Process

Generates an animation of a data stream clustering process.

## Usage

``` r
animate_cluster(
  dsc,
  dsd,
  measure = NULL,
  horizon = 100,
  n = 1000,
  type = c("auto", "micro", "macro"),
  assign = "micro",
  assignmentMethod = c("auto", "model", "nn"),
  excludeNoise = FALSE,
  wait = 0.1,
  plot.args = NULL,
  ...
)
```

## Arguments

- dsc:

  a [DSC](http://michael.hahsler.net/stream/reference/DSC.md)

- dsd:

  a [DSD](http://michael.hahsler.net/stream/reference/DSD.md)

- measure:

  the evaluation measure that should be graphed below the animation (see
  [`evaluate_stream()`](http://michael.hahsler.net/stream/reference/evaluate.md).)

- horizon:

  the number of points displayed at once/used for evaluation.

- n:

  the number of points to be plotted

- type, assign, assignmentMethod, excludeNoise:

  are passed on to
  [`evaluate_stream()`](http://michael.hahsler.net/stream/reference/evaluate.md)
  to calculate the evaluation measure.

- wait:

  the time interval between each frame

- plot.args:

  a list with plotting parameters for the clusters.

- ...:

  extra arguments are added to `plot.args`.

## Details

Animations are recorded using the library animation and can be replayed
(which gives a smoother experience since the is no more computation
done) and saved in various formats (see Examples section below).

**Note:** You need to install package animation and its system
requirements.

## See also

[`animation::ani.replay()`](https://rdrr.io/pkg/animation/man/ani.record.html)
for replaying and saving animations.

Other DSC:
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md),
[`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md),
[`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md),
[`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md),
[`DSC_Static()`](http://michael.hahsler.net/stream/reference/DSC_Static.md),
[`DSC_TwoStage()`](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md),
[`evaluate.DSC`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md),
[`get_assignment()`](http://michael.hahsler.net/stream/reference/get_assignment.md),
[`plot.DSC()`](http://michael.hahsler.net/stream/reference/plot.DSC.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`prune_clusters()`](http://michael.hahsler.net/stream/reference/prune_clusters.md),
[`read_saveDSC`](http://michael.hahsler.net/stream/reference/read_saveDSC.md),
[`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)

Other plot:
[`animate_data()`](http://michael.hahsler.net/stream/reference/animate_data.md),
[`plot.DSC()`](http://michael.hahsler.net/stream/reference/plot.DSC.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md)

Other evaluation:
[`evaluate`](http://michael.hahsler.net/stream/reference/evaluate.md),
[`evaluate.DSC`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md)

## Author

Michael Hahsler

## Examples

``` r
if (interactive()) {
stream <- DSD_Benchmark(1)

### animate the clustering process with evaluation
### Note: we choose to exclude noise points from the evaluation
###       measure calculation, even if the algorithm would assign
###       them to a cluster.
dbstream <- DSC_DBSTREAM(r = .04, lambda = .1, gaptime = 100, Cm = 3,
  shared_density = TRUE, alpha = .2)

animate_cluster(dbstream, stream, horizon = 100, n = 5000,
  measure = "crand", type = "macro", assign = "micro",
  plot.args = list(xlim = c(0, 1), ylim = c(0, 1), shared = TRUE))
}
```
