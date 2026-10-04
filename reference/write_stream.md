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
#>  [3] "0.237760913250896,0.690993092687568,0.973500796847616,0.886690053288229,0.653903434814334,2"
#>  [4] "0.124194689292024,0.798997346541385,0.48974688492934,0.680305851328416,0.983699268530413,1" 
#>  [5] "0.165668039869027,0.758790541282164,0.498479860239797,0.826996157122082,0.994668577447848,1"
#>  [6] "0.697573911399186,0.802025558011091,0.737290086025913,0.583869546864972,0.325574273599949,3"
#>  [7] "0.738479733407413,0.83235137253593,0.744451266868606,0.615392907976226,0.362898269906605,3" 
#>  [8] "0.249819984191527,0.717115930394168,1.02236681407669,0.955840647026976,0.586115230953827,2" 
#>  [9] "0.694234709750454,0.775868933268908,0.717607633773062,0.644221307194689,0.345447544258245,3"
#> [10] "0.733289440504804,0.789596819392345,0.696153989340352,0.567270641203755,0.390641710118243,3"
#> [11] "0.700162752285796,0.823758154001112,0.715381038253656,0.66008147222186,0.359800923110646,3" 

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
#> [2] "0.693118848239676,0.813237007856467,0.702619868905343,0.673681184907583,0.408052914646634,3"
#> [3] "0.667137262008993,0.727288204258201,0.664663103953725,0.656076454934904,0.403378955184659,3"
#> [4] "0.723833303017779,0.784432153053699,0.757296675627418,0.611491425037084,0.380334627057526,3"
#> [5] "0.191891020153855,0.646277004148892,1.03094173859594,0.823134142497782,0.578630981079458,2" 
#> [6] "0.698547704635825,0.771534291722174,0.695679262055455,0.620557710822163,0.381384828893982,3"

# clean up
file.remove("data.txt")
#> [1] TRUE
```
