# stream 1.4-0.1 (xx/xx/20)

## New Features
* DSC implementations are now registered using DSC_registry.

## Bug Fixes
* Fixed get_assignment.DSC_TwoStage when new data is avaialble (reported by 
ozlempoyraz).

# stream 1.4-0 (12/01/20)

## New Features
* Added additional features to the DSD_Gaussians, now capable of using Mahalanobis distance and
  generating outliers.
* Updated evaluation procedure, now capable of performing external indices calculation using callbacks
* Added support for single-pass clusterers and outlier detectors
* Added outlier correctness assessment indices

# stream 1.3-2 (05/04/20)

## Bug Fixes
* DBSTREAM: Fixed array index bug (reported by MatthiasCarnein) 
* BIRCH: Fixed C++ this pointer problem.

# stream 1.3-1 (06/07/19)

## New Features
* Added DSC_evoStream and DSC_EA. Code by Matthias Carnein.

## Changes
* Package animation is now only suggested since it requires package magick 
    which may need the imagemagick++ libraries installed. 

# stream 1.3-0 (05/31/18)

## New Features
* Added DSC_BIRCH. Code and Interface by Dennis Assenmacher and Matthias Carnein.
* Added DSC_BICO. Code by Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, 
    Chris Schwiegelshohn, Christian Sohler and Interface provided by Matthias 
    Carnein and Dennis Assenmacher.
* animate_cluster: noise now accepts "class" or "exclude" ("ignore" is deprecated).

## Bug Fixes
* DSD_ReadCSV: Fixed bug with streams that have no class/cluster label 
    (reported by Matthias Carnein).

# stream 1.2-4 (02/25/17)

## Bug Fixes
* Use dbFetch in DSD_ReadDB (new version of RSQLite).
* Register native C routines.

# stream 1.2-3 (08/07/16)

## Bug Fixes
* fixed saveDSC for DBStream.
* fixed handling of data with d=1 (reported by Ilana Lichtenstein).
* plot now automatically determines if the data supports a class attribute.

# stream 1.2-2 (10/28/15) 

* evaluate now reports noise information.

# stream 1.2-1 (09/08/15)

* fixed problem with failing test under Windows.

# stream 1.2-0 (09/06/15)

* generic and methods for description() added to exact descriptions from
        DSD, DSC and DSO objects.
* write_stream() gained parameter append and now throws an error if it
	      would overwrite a file.
* DSC objects can now be saved and loaded using saveDSC and readDSC.
* we use now DBSCAN from package dbscan.
* DSC_DBSTREAM gained parameter metric and now also supports
        Manhattan and Maximum norm.
* DSC_DBSTREAM gained parameter assignments and function
        get_cluster_assignments() to retrieve the MC assignment of the
        clustered data points.
* cleaned up interface for animate_cluster() and animate_data().
* DSD_ReadCSV was completely rewritten to be more reliable. Lost argument d
        which is now figured out automatically.
* write_stream has now an argument called header (former name was col.names)
        to be consistent with DSD_ReadCSV.

# stream 1.1-5 (07/02/15)

* NAMESPACE now imports non-standard packages correctly.
* DSC_DBSTREAM uses now Cm instead of noise.
* fixed iterator bug for DSC_DBSTREAM.
* evaluate gains argument noise to control if noise is ignored

# stream 1.1-4 (05/24/15)

* evaluate checks if DSD has cluster labels for external
	      evaluation measures.
* DSD_mlbenchmarkGenerator now shuffles data points.
* DSC_ReadCSV gains arguments skip and header.
* DSC_DStream: was reimplemented in C++ (Rcpp),
        number of grids N can now be fixed by the user.
* DSC_tNN was renamed DSC_DBSTREAM. Uses now SOM-style micro-cluster update
        and was reimplemented in C++ (Rcpp).

# stream 1.1-1 (01/15/15)

* DSC_DStream: fixed bug with removing too many sporadic grids
* DSD_ReadCSV now uses readLine so it can read properly from URLs
* updated vignette

# stream 1.1-0 (12/18/14)

* update now directly dispatches
* DSC_Memory replaces DSD_Wrapper
* DSD_ReadCSV replaces DSD_ReadStream. Improved handling of blocking and
      end of stream.
* added DSD_ReadDB (DBI interface)
* get_points can now produce cluster and class information

# stream 1.0-3 (07/14/14)

* Fixed precision and recall calculation
* Added DSC_TwoStage

# stream 1.0-2 (06/16/14)

* Warning for reclusterers removed.
* plot can now show micro-cluster assignment areas using assignment=TRUE

# stream 1.0-1 (06/12/14)

* Improved documentation
* Improved DSD_MG
* plot gained a dim argument to plot only selected dimensions
* get_assignment gained a threshold argument
* DSC_Window added
* DSC_Sample gained a biased argument for biased sampling
* DSC_Wrapper can now wrap matrix-like objects (e.g., from package ff and
        bigmemory)

# stream 1.0-0 (5/24/14)

* added D-Stream (with attraction)
* improved support for creating animations
* tnn: new decay models, tNN without shared density now reclusters
        using density reachability
* plot gained the type "both" that plots micro and macro-clusters
* DSC_Hierarchical and DSC_Kmeans gained min_weight to filter low weight
        micro-clusters before reclustering
* removed default radius, etc. for most clustering algorithms
* Added DSD_MG for simulating streams with concept drift
* moved MOA related code to streamMOA
* suspended DSC_BIRCH because of memory issues
* reset_stream gained a pos argument

# stream 0.2-0 (2/21/14)

* major restructuring

#  stream 0.1-1 (8/16/13)

* initial version
