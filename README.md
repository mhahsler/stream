
# <img src="man/figures/logo.svg" align="right" height="139" /> R package stream - Infrastructure for Data Stream Mining

[![CRAN
version](http://www.r-pkg.org/badges/version/stream)](https://CRAN.R-project.org/package=stream)
[![stream r-universe
status](https://mhahsler.r-universe.dev/badges/stream)](https://mhahsler.r-universe.dev/stream)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/stream)](https://CRAN.R-project.org/package=stream)

The package provides support for modeling and simulating data streams as
well as an extensible framework for implementing, interfacing and
experimenting with algorithms for various data stream mining tasks. The
main advantage of stream is that it seamlessly integrates with the large
existing infrastructure provided by R. The package provides:

- **Stream Sources:** streaming from files, databases, in-memory data,
  URLs, pipes, socket connections and several data stream generators
  including dynamically streams with concept drift.
- **Stream Processing** with filters (convolution, scaling, exponential
  moving average, …)
- **Stream Aggregation:** sampling, windowing.
- **Stream Clustering:** **BICO**, **BIRCH**, **D-Stream**,
  **DBSTREAM**, and **evoStream**.
- **Stream Outlier Detection** based on **D-Stream**, **DBSTREAM**.
- **Stream Classification** with **DecisionStumps**, **HoeffdingTree**,
  **NaiveBayes** and **Ensembles** (streamMOA via RMOA).
- **Stream Regression** with **Perceptron**, **FIMTDD**, **ORTO**, …
  (streamMOA via RMOA).
- **Stream Mining Evaluation** with prequential error estimation.

Additional packages in the stream family are:

- [streamMOA](https://github.com/mhahsler/streamMOA): Interface to
  clustering algorithms implemented in the
  [MOA](https://moa.cms.waikato.ac.nz/) framework. The package
  interfaces clustering algorithms like of **DenStream**, **ClusTree**,
  **CluStream** and **MCOD**. The package also provides an interface to
  [RMOA](https://github.com/jwijffels/RMOA) for MOA’s stream classifiers
  and stream regression models.
- [rEMM](https://github.com/mhahsler/rEMM): Provides implementations of
  **threshold nearest neighbor clustering** (tNN) and **Extensible
  Markov Model** (EMM) for modelling temporal relationships between
  clusters.

## Installation

**Stable CRAN version:** Install from within R with

``` r
install.packages("stream")
```

**Current development version:** Install from
[r-universe.](https://mhahsler.r-universe.dev/stream)

``` r
install.packages("stream", repos = "https://mhahsler.r-universe.dev")
```

## Usage

Load the package and a random data stream with 3 Gaussian clusters and
10% noise and scale the data to z-scores.

``` r
library("stream")
set.seed(2000)

stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.1) %>%
    DSF_Scale()
get_points(stream, n = 5)
```

    ##       X1     X2 .class
    ## 1 -0.267 -0.802      2
    ## 2  0.531  1.078     NA
    ## 3 -0.706  1.427      3
    ## 4 -0.781  1.355      3
    ## 5  1.170 -0.712      1

``` r
plot(stream)
```

![](inst/README_files/stream-1.png)<!-- -->

Cluster a stream of 1000 points using D-Stream which estimates point
density in grid cells.

``` r
dsc <- DSC_DStream(gridsize = 0.1)
update(dsc, stream, 1000)
plot(dsc, stream, grid = TRUE)
```

![](inst/README_files/Dstream-1.png)<!-- -->

``` r
evaluate_static(dsc, stream, n = 100)
```

    ## Evaluation results for micro-clusters.
    ## Points were assigned to micro-clusters.
    ## 
    ##             numPoints      numMicroClusters      numMacroClusters 
    ##              100.0000               65.0000                3.0000 
    ##        noisePredicted                   SSQ            silhouette 
    ##               23.0000                0.1696                0.0786 
    ##       average.between        average.within          max.diameter 
    ##                1.7809                0.5816                3.9368 
    ##        min.separation ave.within.cluster.ss                    g2 
    ##                0.0146                0.5217                0.1596 
    ##          pearsongamma                  dunn                 dunn2 
    ##                0.0637                0.0037                0.0154 
    ##               entropy              wb.ratio            numClasses 
    ##                3.1721                0.3266                4.0000 
    ##           noiseActual        noisePrecision        outlierJaccard 
    ##               16.0000                0.6957                0.6957 
    ##             precision                recall                    F1 
    ##                0.6170                0.1618                0.2563 
    ##                purity             Euclidean             Manhattan 
    ##                0.9920                0.1633                0.3000 
    ##                  Rand                 cRand                   NMI 
    ##                0.7620                0.1688                0.5551 
    ##                    KP                 angle                  diag 
    ##                0.2651                0.3000                0.3000 
    ##                    FM               Jaccard                    PS 
    ##                0.3159                0.1470                0.0541 
    ##                    vi 
    ##                2.2264 
    ## attr(,"type")
    ## [1] "micro"
    ## attr(,"assign")
    ## [1] "micro"

Outlier detection using DBSTREAM which uses micro-clusters with a given
radius.

``` r
dso <- DSOutlier_DBSTREAM(r = 0.1)
update(dso, stream, 1000)
plot(dso, stream)
```

![](inst/README_files/DSOutlier_DBSTREAM-1.png)<!-- -->

``` r
evaluate_static(dso, stream, n = 100, measure = c("numPoints", "noiseActual", "noisePredicted",
    "noisePrecision"))
```

    ## Evaluation results for micro-clusters.
    ## Points were assigned to micro-clusters.
    ## 
    ##      numPoints    noiseActual noisePredicted noisePrecision 
    ##            100              7              7              1 
    ## attr(,"type")
    ## [1] "micro"
    ## attr(,"assign")
    ## [1] "micro"

Preparing complete stream process pipelines that can be run using a
single `update()` call.

``` r
pipeline <- DSD_Gaussians(k = 3, d = 2, noise = 0.1) %>%
    DSF_Scale() %>%
    DST_Runner(DSC_DStream(gridsize = 0.1))
pipeline
```

    ## DST pipline runner
    ## DSD: Gaussian Mixture (d = 2, k = 3)
    ## + scaled
    ## DST: D-Stream 
    ## Class: DST_Runner, DST

``` r
update(pipeline, n = 500)
```

    ##     weight    X1    X2
    ## 1    0.812 -1.75 -0.65
    ## 2    0.888 -1.75 -0.55
    ## 3    1.738 -1.65 -1.05
    ## 4    0.865 -1.65 -0.75
    ## 5    3.333 -1.65 -0.65
    ## 6    1.890 -1.65 -0.55
    ## 7    1.677 -1.55 -0.95
    ## 8    1.590 -1.55 -0.85
    ## 9    1.432 -1.55 -0.55
    ## 10   0.773 -1.55 -0.45
    ## 11   3.288 -1.45 -0.95
    ## 12   1.712 -1.45 -0.85
    ## 13   3.466 -1.45 -0.75
    ## 14   2.514 -1.45 -0.65
    ## 15   1.582 -1.35 -1.15
    ## 16   1.804 -1.35 -1.05
    ## 17   4.806 -1.35 -0.95
    ## 18   5.170 -1.35 -0.85
    ## 19   2.521 -1.35 -0.65
    ## 20   0.803 -1.35 -0.55
    ## 21   0.973 -1.25 -1.15
    ## 22   0.842 -1.25 -1.05
    ## 23   4.945 -1.25 -0.95
    ## 24   4.176 -1.25 -0.85
    ## 25   4.267 -1.25 -0.75
    ## 26   3.513 -1.25 -0.65
    ## 27   1.585 -1.25 -0.55
    ## 28   0.961 -1.25 -0.45
    ## 29   0.825 -1.15 -1.15
    ## 30   1.819 -1.15 -1.05
    ## 31   0.846 -1.15 -0.95
    ## 32   3.410 -1.15 -0.85
    ## 33   1.716 -1.15 -0.75
    ## 34   0.765 -1.05 -1.15
    ## 35   4.891 -1.05 -1.05
    ## 36   2.603 -1.05 -0.95
    ## 37   6.110 -1.05 -0.85
    ## 38   0.957 -1.05 -0.65
    ## 39   1.895 -1.05 -0.55
    ## 40   0.788 -1.05 -0.45
    ## 41   2.581 -0.95 -1.15
    ## 42   2.556 -0.95 -1.05
    ## 43   3.479 -0.95 -0.95
    ## 44   4.340 -0.95 -0.85
    ## 45   2.560 -0.95 -0.75
    ## 46   0.845 -0.95 -0.55
    ## 47   2.898 -0.85 -1.05
    ## 48   2.616 -0.85 -0.95
    ## 49   2.545 -0.85 -0.85
    ## 50   1.556 -0.75 -1.05
    ## 51   0.965 -0.75 -0.95
    ## 52   0.771 -0.65 -1.05
    ## 53   0.930 -0.05  1.15
    ## 54   1.656  0.05  0.95
    ## 55   3.891  0.05  1.05
    ## 56   1.738  0.05  1.15
    ## 57   0.891  0.05  1.35
    ## 58   0.769  0.15  0.85
    ## 59   1.886  0.15  0.95
    ## 60   2.487  0.15  1.05
    ## 61   3.342  0.15  1.15
    ## 62   4.257  0.15  1.25
    ## 63   1.833  0.15  1.35
    ## 64   0.877  0.15  1.45
    ## 65   0.886  0.25  0.75
    ## 66   0.763  0.25  0.85
    ## 67   3.426  0.25  0.95
    ## 68   3.979  0.25  1.05
    ## 69   5.465  0.25  1.15
    ## 70   1.778  0.25  1.25
    ## 71   0.776  0.25  1.45
    ## 72   1.739  0.35 -0.35
    ## 73   0.749  0.35 -0.25
    ## 74   1.651  0.35 -0.15
    ## 75   0.959  0.35  0.65
    ## 76   2.146  0.35  0.75
    ## 77   1.510  0.35  0.85
    ## 78   2.335  0.35  0.95
    ## 79   5.286  0.35  1.05
    ## 80   5.187  0.35  1.15
    ## 81   3.046  0.35  1.25
    ## 82   3.268  0.35  1.35
    ## 83   1.489  0.35  1.45
    ## 84   0.957  0.35  1.55
    ## 85   0.757  0.45 -0.55
    ## 86   3.486  0.45 -0.45
    ## 87   1.584  0.45 -0.35
    ## 88   1.948  0.45 -0.25
    ## 89   4.212  0.45 -0.15
    ## 90   1.520  0.45 -0.05
    ## 91   0.722  0.45  0.65
    ## 92   2.474  0.45  0.75
    ## 93   2.579  0.45  0.85
    ## 94   3.733  0.45  0.95
    ## 95   4.920  0.45  1.05
    ## 96   3.280  0.45  1.15
    ## 97   1.693  0.45  1.25
    ## 98   1.735  0.45  1.35
    ## 99   0.945  0.45  1.45
    ## 100  0.821  0.55 -0.55
    ## 101  2.516  0.55 -0.45
    ## 102  4.479  0.55 -0.35
    ## 103  1.700  0.55 -0.25
    ## 104  1.714  0.55  0.85
    ## 105  4.318  0.55  0.95
    ## 106  2.532  0.55  1.05
    ## 107  1.520  0.55  1.15
    ## 108  2.543  0.55  1.25
    ## 109  1.651  0.55  1.35
    ## 110  0.785  0.55  1.45
    ## 111  0.980  0.65 -0.65
    ## 112  0.791  0.65 -0.55
    ## 113  5.131  0.65 -0.45
    ## 114  7.010  0.65 -0.35
    ## 115  0.758  0.65 -0.15
    ## 116  1.573  0.65  0.75
    ## 117  0.837  0.65  0.85
    ## 118  1.638  0.65  0.95
    ## 119  5.988  0.65  1.05
    ## 120  1.602  0.65  1.15
    ## 121  1.776  0.65  1.25
    ## 122  0.861  0.65  1.35
    ## 123  0.849  0.75 -0.85
    ## 124  0.893  0.75 -0.75
    ## 125  2.393  0.75 -0.65
    ## 126  2.672  0.75 -0.55
    ## 127  3.460  0.75 -0.45
    ## 128  5.000  0.75 -0.35
    ## 129  2.555  0.75 -0.25
    ## 130  2.245  0.75 -0.15
    ## 131  1.807  0.75  0.15
    ## 132  2.679  0.75  0.95
    ## 133  0.927  0.75  1.05
    ## 134  2.551  0.75  1.15
    ## 135  0.746  0.75  1.25
    ## 136  1.761  0.85 -0.85
    ## 137  0.798  0.85 -0.65
    ## 138  1.926  0.85 -0.55
    ## 139  0.826  0.85 -0.45
    ## 140  2.570  0.85 -0.25
    ## 141  0.923  0.85 -0.15
    ## 142  1.671  0.85 -0.05
    ## 143  0.818  0.85  0.95
    ## 144  0.774  0.85  1.15
    ## 145  0.770  0.95 -0.75
    ## 146  1.576  0.95 -0.65
    ## 147  0.733  0.95 -0.55
    ## 148  1.654  0.95 -0.45
    ## 149  4.020  0.95 -0.35
    ## 150  1.855  0.95 -0.25
    ## 151  1.540  1.05 -0.75
    ## 152  0.802  1.05 -0.65
    ## 153  2.753  1.05 -0.55
    ## 154  1.490  1.05 -0.45
    ## 155  1.586  1.05 -0.35
    ## 156  2.697  1.05 -0.25
    ## 157  0.911  1.05 -0.15
    ## 158  1.662  1.15 -0.65
    ## 159  0.781  1.15 -0.45
    ## 160  0.883  1.15 -0.25

``` r
pipeline$dst
```

    ## D-Stream 
    ## Class: DSC_DStream, DSC_Micro, DSC_R, DSC 
    ## Number of micro-clusters: 160 
    ## Number of macro-clusters: 13

## Acknowledgements

The development of the stream package was supported in part by NSF
IIS-0948893, NSF CMMI 1728612, and NIH R21HG005912.

## References

- Michael Hahsler, Matthew Bolaños, and John Forrest. [stream: An
  extensible framework for data stream clustering research with
  R.](https://dx.doi.org/10.18637/jss.v076.i14) *Journal of Statistical
  Software,* 76(14), February 2017.
- [stream package
  vignette](https://cran.r-project.org/package=stream/vignettes/stream.pdf)
  with complete examples.
- [stream reference
  manual](https://cran.r-project.org/package=stream/stream.pdf)
