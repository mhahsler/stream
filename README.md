# stream - Infrastructure for Data Stream Mining - R package

[![CRAN version](http://www.r-pkg.org/badges/version/stream)](https://cran.r-project.org/package=stream)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/stream)](https://cran.r-project.org/package=stream)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/stream.svg?branch=master)](https://travis-ci.org/mhahsler/stream)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/stream?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/stream)

A framework for data stream modeling and associated data mining tasks such as clustering and classification. The development of this package was supported in part by NSF IIS-0948893 and NIH R21HG005912.

Additional packages in the stream family are: [streamMOA](http://github.com/mhahsler/streamMOA). 

## Installation

* __Stable CRAN version:__ install from within R.
* __Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/stream/build/artifacts) or install via `install_git("mhahsler/stream")` (needs devtools) 

## Example
```R
R> library("stream")
R> stream <- DSD_Gaussians(k=3, noise=0)

# create micro-clusters via sampling
R> sample <- DSC_Sample(k=20)
R> update(sample, stream, 500)
R> sample
Reservoir sampling
Class: DSC_Sample, DSC_Micro, DSC_R, DSC 
Number of micro-clusters: 20 

# recluster micro-clusters
R> kmeans <- DSC_Kmeans(k=3)
R> recluster(kmeans, sample)
R> plot(kmeans, stream, type="both")
```

## Further Information

* Development version of [stream on github](https://github.com/mhahsler/stream).
* [stream package vignette](http://cran.r-project.org/web/packages/stream/vignettes/stream.pdf) with complete examples.
* [stream reference manual](http://cran.r-project.org/web/packages/stream/stream.pdf)

_Maintainer:_ [Michael Hahsler](http://michael.hahsler.net)

