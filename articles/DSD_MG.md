# The DSD Moving Generator

[`DSD_MG()`](https://michael.hahsler.net/stream/reference/DSD_MG.md)
combines Moving Generator Clusters (`MGC`s) into an evolving data
stream. Each component specifies a cluster’s location, shape, size, and
density over time. This guide expands on the generator introduced in the
original `stream_vignette.Rnw`, using examples from the `DSD_MG` and
`MGC` help pages.

## Combine clusters and noise

Start with a Gaussian cluster, a uniform rectangular cluster, and
uniform background noise. All components must have the stream’s
dimensionality. `parameter` gives standard deviations for
`Shape_Gaussian` and side lengths for `Shape_Block`.

``` r

stream <- DSD_MG(dimension = 2,
  MGC_Static(density = 45, center = c(1, 0), parameter = 0.1),
  MGC_Static(density = 45, center = c(2, 0), parameter = c(0.4, 0.6),
    shape = Shape_Block),
  MGC_Noise(density = 10, range = rbind(c(0, 3), c(-1, 1))),
  labels = c(1, 2, NA)
)
stream
#> Moving Data GeneratorClass: DSD_MG, DSD_R, DSD 
#> With 3 clusters in 2 dimensions. Time is 1
get_points(stream, n = 5)
#>         X1          X2 .class
#> 1 1.827095  0.14322891      2
#> 2 1.021095 -0.06553463      1
#> 3 2.102185 -0.10974945      2
#> 4 1.110681 -0.14546372      1
#> 5 1.034556  0.02119348      1
plot(stream, n = 500, xlim = c(0, 3), ylim = c(-1, 1))
```

![](DSD_MG_files/figure-html/static-stream-1.png)

`density` controls both the relative chance of choosing a component and
the number of points generated per unit of generator time. Here the
total density is 100, so approximately 100 points advance the generator
by one time unit. Noise has label `NA` in the `.class` column. Supply
the same label to multiple components when they should represent one
ground-truth cluster.

## Define movement with keyframes

[`MGC_Linear()`](https://michael.hahsler.net/stream/reference/MGC.md)
interpolates the center, density, and shape parameters between
keyframes. The next example starts with one cluster. At time 50, a
second component appears at the same location, then moves in another
direction.

``` r

c1 <- MGC_Linear(dimension = 2, keyframelist = list(
  keyframe(time = 1, density = 20, center = c(0, 0), parameter = 5),
  keyframe(time = 50, density = 20, center = c(50, 50), parameter = 5),
  keyframe(time = 100, density = 20, center = c(50, 100), parameter = 5)
))
c2 <- MGC_Linear(dimension = 2, keyframelist = list(
  keyframe(time = 50, density = 20, center = c(50, 50), parameter = 5),
  keyframe(time = 100, density = 20, center = c(100, 50), parameter = 5)
))
stream <- DSD_MG(dimension = 2, c1, c2)
get_keyframes(c1)
#>   time parameter density centers reset
#> 1    1         5      20    0, 0 FALSE
#> 2   50         5      20  50, 50 FALSE
#> 3  100         5      20 50, 100 FALSE
```

A linear component produces no points before its first keyframe. After
its last keyframe it keeps its final attributes. A keyframe with
`reset = TRUE` can make the trajectory repeat, as in `DSD_Benchmark(1)`.

Generator time is different from the count of requested points. The
second component starts after about 980 points: 49 time units at density
20. Once it appears, the total density becomes 40. Plot three short
sections around this change and near the end of the movement:

``` r

local({
  old_par <- par(mfrow = c(1, 3))
  on.exit(par(old_par))
  plot(stream, n = 200, xlim = c(-15, 115), ylim = c(-15, 115),
    main = "One cluster")
  invisible(get_points(stream, n = 780))
  plot(stream, n = 200, xlim = c(-15, 115), ylim = c(-15, 115),
    main = "Second cluster appears")
  invisible(get_points(stream, n = 1400))
  plot(stream, n = 200, xlim = c(-15, 115), ylim = c(-15, 115),
    main = "Separate paths")
})
```

![](DSD_MG_files/figure-html/splitting-snapshots-1.png)

## Add and remove components

Generators can be modified after construction.
[`get_clusters()`](https://michael.hahsler.net/stream/reference/DSD_MG.md)
returns the component list;
[`remove_cluster()`](https://michael.hahsler.net/stream/reference/DSD_MG.md)
takes an index in that list.

``` r

noise <- MGC_Noise(density = 2, range = rbind(c(-15, 115), c(-15, 115)))
add_cluster(stream, noise, label = NA)
length(get_clusters(stream))
#> [1] 3
plot(stream, n = 500, xlim = c(-15, 115), ylim = c(-15, 115))
```

![](DSD_MG_files/figure-html/modify-stream-1.png)

``` r

remove_cluster(stream, i = 3)
length(get_clusters(stream))
#> [1] 2
```

Use
[`add_keyframe()`](https://michael.hahsler.net/stream/reference/MGC.md)
and
[`remove_keyframe()`](https://michael.hahsler.net/stream/reference/MGC.md)
to edit a linear component’s trajectory. Stream and component objects
contain mutable state: updating a component can also affect a stream
that contains it.

## Specify movement as a function of time

[`MGC_Function()`](https://michael.hahsler.net/stream/reference/MGC.md)
accepts functions of generator time for density, center, and shape
parameters. Here a Gaussian cluster travels around a circle while a
static cluster remains at the center.

``` r

orbit <- MGC_Function(
  density = function(t) 20,
  center = function(t) c(0.5 + 0.35 * cos(t / 10),
    0.5 + 0.35 * sin(t / 10)),
  parameter = function(t) 0.03
)
stream <- DSD_MG(dimension = 2, orbit,
  MGC_Static(density = 5, center = c(0.5, 0.5), parameter = 0.03))
```

The GIF below shows the functional generator in batches of 50 points,
making the orbiting cluster’s movement visible. It loops automatically
at ten frames per second and is embedded directly in the HTML.

Knitr creates the GIF using the `gifski` R package. Install `gifski`
before rendering to include the animation. The animation chunk is
skipped when `gifski` is unavailable or the output is not HTML.

``` r

local({
  set.seed(1000)
  reset_stream(stream)
  for (i in seq_len(30)) {
    plot(stream, n = 50, xlim = c(0, 1), ylim = c(0, 1),
      main = paste("Moving Generator after", i * 50, "points"))
  }
})
```

![A moving Gaussian cluster orbiting a static
cluster](DSD_MG_files/figure-html/generator-animation-.gif)

For interactive exploration, the following example uses the `animation`
package and is not run during vignette builds.

``` r

reset_stream(stream)
animate_data(stream, n = 1500, horizon = 100,
  xlim = c(0, 1), ylim = c(0, 1))
animation::ani.replay()
```
