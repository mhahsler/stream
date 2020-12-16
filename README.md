# stream - Infrastructure for Data Stream Mining - R package

[![CRAN version](http://www.r-pkg.org/badges/version/stream)](https://cran.r-project.org/package=stream)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/stream)](https://cran.r-project.org/package=stream)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/stream.svg?branch=master)](https://travis-ci.org/mhahsler/stream)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/stream?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/stream)

The package provides support for modeling and simulating data streams as well as an extensible framework for implementing, interfacing and
experimenting with algorithms for various data stream mining tasks. The main advantage of stream is that it seamlessly integrates with the large existing infrastructure provided by R. The package currently focuses on data stream clustering and provides
implementations of **BICO**, **BIRCH**, **D-Stream**, **DBSTREAM**, and **evoStream**. 

Additional packages in the stream family are: 

* [streamMOA](https://github.com/mhahsler/streamMOA): Interface to clustering
  algorithms implemented in the [MOA](https://moa.cms.waikato.ac.nz/) framework.
  Includes implementations of **DenStream**, **ClusTree** and **CluStream**.
* [subspaceMOA](https://cran.r-project.org/package=subspaceMOA):
  Interface to Subspace MOA and
  its implementations of **HDDStream** and **PreDeConStream**.

The development of the stream package was supported in part by NSF IIS-0948893 and NIH R21HG005912.

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("stream")
```
__Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/stream/build/artifacts) or install from GitHub (needs devtools).
```R 
install_git("mhahsler/stream")
```



## Usage

Load the package and create micro-clusters via sampling.

```R
library("stream")
stream <- DSD_Gaussians(k=3, noise=0)

sample <- DSC_Sample(k=20)
update(sample, stream, 500)
sample
```

```
Reservoir sampling
Class: DSC_Sample, DSC_Micro, DSC_R, DSC 
Number of micro-clusters: 20 
```

Recluster micro-clusters using k-means and plot results

```R
kmeans <- DSC_Kmeans(k=3)
recluster(kmeans, sample)
plot(kmeans, stream, type="both")
```

A list of all available clustering methods can be obtained with

```
DSC_registry$get_entries()
```



## References

* Michael Hahsler, Matthew Bolaños, and John Forrest. [stream: An extensible framework for data stream clustering research with R.](http://dx.doi.org/10.18637/jss.v076.i14) _Journal of Statistical Software,_ 76(14), February 2017. 
* [stream package vignette](https://cran.r-project.org/package=stream/vignettes/stream.pdf) with complete examples.
* [stream reference manual](https://cran.r-project.org/package=stream/stream.pdf)
