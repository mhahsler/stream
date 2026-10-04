# DSClassifier_SlidingWindow – Data Stream Classifier Using a Sliding Window

The classifier keeps a sliding window for the stream and rebuilds a
classification model at regular intervals. By default is builds a
decision tree using
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html).

## Usage

``` r
DSClassifier_SlidingWindow(formula, model = rpart::rpart, window, rebuild, ...)
```

## Arguments

- formula:

  a formula for the classification problem.

- model:

  classifier model (that has a formula interface).

- window:

  size of the sliding window.

- rebuild:

  interval (number of points) for rebuilding the classifier. Set rebuild
  to `Inf` to prevent automatic rebuilding. Rebuilding can be initiated
  manually when calling
  [`update()`](http://michael.hahsler.net/stream/reference/update.md).

- ...:

  additional parameters are passed on to the classifier (default is
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html)).

## Value

An object of class `DST_SlidingWindow`.

## Details

This constructor creates classifier based on
[`DST_SlidingWindow`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md).
The classifier has a
[`update()`](http://michael.hahsler.net/stream/reference/update.md) and
[`predict()`](http://michael.hahsler.net/stream/reference/predict.md)
method.

## See also

Other DSClassifier:
[`DSClassifier()`](http://michael.hahsler.net/stream/reference/DSClassifier.md)

## Author

Michael Hahsler

## Examples

``` r
library(stream)

# create a data stream for the iris dataset
data <- iris[sample(nrow(iris)), ]
stream <- DSD_Memory(data)

# define the stream classifier.
cl <- DSClassifier_SlidingWindow(
  Species ~ Sepal.Length + Sepal.Width + Petal.Length,
  window = 50,
  rebuild = 10
  )
cl
#> Data Stream Classifier on a Sliding Window
#> Function: rpart::rpart 
#> Class: DSClassifier_SlidingWindow, DSClassifier, DST_SlidingWindow, DST 

# update the classifier with 100 points from the stream
update(cl, stream, 100)

# predict the class for the next 50 points
newdata <- get_points(stream, n = 50)
pr <- predict(cl, newdata, type = "class")
pr
#>         47        130         50        115         84        103         20 
#>     setosa  virginica     setosa  virginica  virginica  virginica     setosa 
#>         14         25        129         42         86        132        148 
#>     setosa     setosa  virginica     setosa versicolor  virginica  virginica 
#>         81        136         36        133         30        149         82 
#> versicolor  virginica     setosa  virginica     setosa  virginica versicolor 
#>        102         59        117         80        124         54         76 
#>  virginica versicolor  virginica versicolor versicolor versicolor versicolor 
#>        141        106        121         37         88         28        114 
#>  virginica  virginica  virginica     setosa versicolor     setosa  virginica 
#>         16        134         72        126         70         93         67 
#>     setosa  virginica versicolor  virginica versicolor versicolor versicolor 
#>        127        108         33          3          7        140         75 
#> versicolor  virginica     setosa     setosa     setosa  virginica versicolor 
#>         78 
#>  virginica 
#> Levels: setosa versicolor virginica

table(pr, newdata$Species)
#>             
#> pr           setosa versicolor virginica
#>   setosa         14          0         0
#>   versicolor      0         13         2
#>   virginica       0          2        19

# get the tree model
get_model(cl)
#> n= 50 
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 50 30 versicolor (0.28000000 0.40000000 0.32000000)  
#>   2) Petal.Length< 2.6 14  0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) Petal.Length>=2.6 36 16 versicolor (0.00000000 0.55555556 0.44444444)  
#>     6) Petal.Length< 5 21  1 versicolor (0.00000000 0.95238095 0.04761905) *
#>     7) Petal.Length>=5 15  0 virginica (0.00000000 0.00000000 1.00000000) *
```
