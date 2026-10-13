# Write a Data Stream to a File

Writes points from a data stream DSD object to a file or a connection.

## Usage

``` r
write_stream(
  dsd,
  file,
  n,
  block = 100000L,
  info = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  close = TRUE,
  ...
)
```

## Arguments

- dsd:

  The DSD object that will generate the data points for output.

- file:

  A file name or a R connection to be written to.

- n:

  The number of data points to be written. For finite streams, `n = -1`
  writes all available data points.

- block:

  Write stream in blocks to improve file I/O speed.

- info:

  Save the class/cluster labels and other information columns with the
  data.

- append:

  Append the data to an existing file. If `FALSE`, then the file will be
  overwritten.

- sep:

  The character that will separate attributes in a data point.

- header:

  A flag that determines if column names will be output (equivalent to
  `col.names` in
  [`write.table()`](https://rdrr.io/r/utils/write.table.html)).

- row.names:

  A flag that determines if row names will be output.

- close:

  close stream after writing.

- ...:

  Additional parameters that are passed to
  [`write.table()`](https://rdrr.io/r/utils/write.table.html).

## Value

There is no value returned from this operation.

## See also

[write.table](https://rdrr.io/r/utils/write.table.html)

## Author

Michael Hahsler

## Examples

``` r
# create data and write 10 points to disk
stream <- DSD_Gaussians(k = 3, d = 5)
stream
#> Gaussian Mixture (d = 5, k = 3) 
#> Class: DSD_Gaussians, DSD_R, DSD 

write_stream(stream, file="data.txt", n = 10, header = TRUE, info = TRUE)

readLines("data.txt")
#>  [1] "\"X1\",\"X2\",\"X3\",\"X4\",\"X5\",\".class\""                                              
#>  [2] "0.129852674246113,0.706690814228115,0.472273650655163,0.720808664911388,1.01199701297213,1" 
#>  [3] "0.23245888614745,0.68869567836618,0.9751517860199,0.888012686738668,0.643464936875555,2"    
#>  [4] "0.124194689292024,0.798997346541385,0.48974688492934,0.680305851328416,0.983699268530413,1" 
#>  [5] "0.165668039869027,0.758790541282164,0.498479860239797,0.826996157122082,0.994668577447848,1"
#>  [6] "0.749729326002502,0.817755489479489,0.716215923008182,0.633875769747636,0.342431748175849,3"
#>  [7] "0.712103759524362,0.824396450410428,0.755108867380279,0.590103820759205,0.354373128029583,3"
#>  [8] "0.29910238833297,0.738470423469728,1.00702085034732,0.943546753585137,0.68314119178193,2"   
#>  [9] "0.692822342401165,0.775442967110236,0.718178321591119,0.642867139904525,0.344991044259854,3"
#> [10] "0.738293767461377,0.791106110818193,0.694131917367979,0.572068752599694,0.392259189627926,3"
#> [11] "0.662651013348963,0.812444715363444,0.730538208557947,0.624115496450051,0.347676521641786,3"

# clean up
file.remove("data.txt")
#> [1] TRUE

# create a finite stream and write all data to disk using n = -1
stream2 <- DSD_Memory(stream, n = 5)
stream2
#> Memorized Stream for Gaussian Mixture (d = 5, k = 3) 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 5 data points - currently at position 1 - loop is FALSE 

write_stream(stream2, file="data.txt", n = -1, header = TRUE, info = TRUE)

readLines("data.txt")
#> [1] "\"X1\",\"X2\",\"X3\",\"X4\",\"X5\",\".class\""                                              
#> [2] "0.634909471743662,0.795681217914722,0.726140224342407,0.617870468473636,0.389238701552484,3"
#> [3] "0.657178071843184,0.724284539538431,0.668687261343234,0.64652765761539,0.40015998364942,3"  
#> [4] "0.725898402225233,0.785054981368952,0.756462241893611,0.613471426768989,0.381002100562456,3"
#> [5] "0.269097601642491,0.679731285418427,1.00690051293266,0.80387433765738,0.730633361679408,2"  
#> [6] "0.696438937631346,0.770898293317875,0.696531340405456,0.618535840731864,0.380703241249921,3"

# clean up
file.remove("data.txt")
#> [1] TRUE
```
