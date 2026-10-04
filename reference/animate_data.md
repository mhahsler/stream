# Animates the Plotting of a Data Streams

Generates an animation of a data stream.

## Usage

``` r
animate_data(dsd, horizon = 100, n = 1000, wait = 0.1, plot.args = NULL, ...)
```

## Arguments

- dsd:

  a DSD object

- horizon:

  the number of points displayed at once/used for evaluation.

- n:

  the number of points to be plotted

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

Other DSD:
[`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md),
[`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md),
[`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md),
[`DSD_Cubes()`](http://michael.hahsler.net/stream/reference/DSD_Cubes.md),
[`DSD_Gaussians()`](http://michael.hahsler.net/stream/reference/DSD_Gaussians.md),
[`DSD_MG()`](http://michael.hahsler.net/stream/reference/DSD_MG.md),
[`DSD_Memory()`](http://michael.hahsler.net/stream/reference/DSD_Memory.md),
[`DSD_Mixture()`](http://michael.hahsler.net/stream/reference/DSD_Mixture.md),
[`DSD_NULL()`](http://michael.hahsler.net/stream/reference/DSD_NULL.md),
[`DSD_ReadDB()`](http://michael.hahsler.net/stream/reference/DSD_ReadDB.md),
[`DSD_ReadStream()`](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md),
[`DSD_Target()`](http://michael.hahsler.net/stream/reference/DSD_Target.md),
[`DSD_UniformNoise()`](http://michael.hahsler.net/stream/reference/DSD_UniformNoise.md),
[`DSD_mlbenchData()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchData.md),
[`DSD_mlbenchGenerator()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md),
[`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md),
[`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md),
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md),
[`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)

Other plot:
[`animate_cluster()`](http://michael.hahsler.net/stream/reference/animate_cluster.md),
[`plot.DSC()`](http://michael.hahsler.net/stream/reference/plot.DSC.md),
[`plot.DSD()`](http://michael.hahsler.net/stream/reference/plot.DSD.md)

## Author

Michael Hahsler

## Examples

``` r
if (interactive()) {

stream <- DSD_Benchmark(1)
animate_data(stream, horizon = 100, n = 5000, xlim = c(0,1), ylim = c(0,1))

### animations can be replayed with the animation package
library(animation)
animation::ani.options(interval = .1) ## change speed
ani.replay()

### animations can also be saved as HTML, animated gifs, etc.
saveHTML(ani.replay())
}
```
