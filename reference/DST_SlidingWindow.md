# DST_SlidingWindow – Call R Functions on a Sliding Window

Keeps a sliding window of the data stream an calls a function at regular
intervals with the contents of the window.

## Usage

``` r
DST_SlidingWindow(f, window, rebuild, ...)

# S3 method for class 'DST_SlidingWindow'
update(
  object,
  dsd,
  n = 1L,
  return = c("nothing", "model"),
  rebuild = FALSE,
  ...
)

# S3 method for class 'DST_SlidingWindow'
predict(object, newdata, ...)
```

## Arguments

- f:

  the function to be called.

- window:

  size of the sliding window.

- rebuild:

  logical; perform a rebuild after the update.

- ...:

  additional parameters passed on to the
  [`predict()`](http://michael.hahsler.net/stream/reference/predict.md)
  function of the underlying model.

- object:

  the updated `DST_SlidingWindow` object.

- dsd:

  A [DSD](http://michael.hahsler.net/stream/reference/DSD.md) object
  with the data stream.

- n:

  number of points from `dsd` to use for the update.

- return:

  a character string indicating what update returns. The default is
  `"nothing"` and `"model"` returns the aggregated data.

- newdata:

  dataframe with the new data.

## Value

An object of class `DST_SlidingWindow`.

## Details

Keeps a sliding window of the data stream an calls a function at regular
intervals with the contents of the window. The function needs to have
the form

`f <- function(data, ...) {...}`

where `data` is the data.frame with the points in the sliding window
(See
[`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md)
in
[DSAggregate_Window](http://michael.hahsler.net/stream/reference/DSAggregate_Window.md)).
The function will be executed at regular intervals after
[`update()`](http://michael.hahsler.net/stream/reference/update.md) was
called with fixed number of points. `update(..., rebuild = TRUE)` can be
used to force recomputing the function. This can be used with `n = 0` to
recompute it even without adding more points. The last valid function
result can be retrieved/

Many modelling functions provide a formula interface which lets them be
directly used inside a `DST_SlidingWindow` (see Examples section).

If the function returns a model that supports
[`predict()`](http://michael.hahsler.net/stream/reference/predict.md),
then predict can directly be called on the `DST_SlidingWindow` object.

## See also

Other DST:
[`DSAggregate()`](http://michael.hahsler.net/stream/reference/DSAggregate.md),
[`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md),
[`DSClassifier()`](http://michael.hahsler.net/stream/reference/DSClassifier.md),
[`DSOutlier()`](http://michael.hahsler.net/stream/reference/DSOutlier.md),
[`DSRegressor()`](http://michael.hahsler.net/stream/reference/DSRegressor.md),
[`DST()`](http://michael.hahsler.net/stream/reference/DST.md),
[`DST_WriteStream()`](http://michael.hahsler.net/stream/reference/DST_WriteStream.md),
[`evaluate`](http://michael.hahsler.net/stream/reference/evaluate.md),
[`predict`](http://michael.hahsler.net/stream/reference/predict.md),
[`stream_pipeline`](http://michael.hahsler.net/stream/reference/stream_pipeline.md),
[`update`](http://michael.hahsler.net/stream/reference/update.md)

## Author

Michael Hahsler

## Examples

``` r
library(stream)

# create a data stream for the iris dataset
data <- iris[sample(nrow(iris)), ]
stream <- DSD_Memory(data)
stream
#> Memorized Stream 
#> Class: DSD_Memory, DSD_R, DSD 
#> Contains 150 data points - currently at position 1 - loop is FALSE 

## Example 1: Use a function on the sliding window
summarizer <- function(data) summary(data)

s <- DST_SlidingWindow(summarizer,
  window = 100, rebuild = 50)
s
#> Data Stream Task: Call function on a Sliding Window
#> Function: summarizer 
#> Class: DST_SlidingWindow, DST 

# update window with 49 points. The function is not yet called
update(s, stream, 49)
get_model(s)
#> NULL

# updating with the 50th point will trigger a function call (see rebuild parameter)
# note that the window is only 1/2 full and we have 50 NAs
update(s, stream, 1)
get_model(s)
#>   Sepal.Length    Sepal.Width    Petal.Length    Petal.Width         Species  
#>  Min.   :4.400   Min.   :2.20   Min.   :1.000   Min.   :0.10   setosa    :16  
#>  1st Qu.:5.125   1st Qu.:2.80   1st Qu.:1.500   1st Qu.:0.20   versicolor:20  
#>  Median :5.700   Median :3.00   Median :4.400   Median :1.30   virginica :14  
#>  Mean   :5.816   Mean   :3.06   Mean   :3.696   Mean   :1.16   NAs       :50  
#>  3rd Qu.:6.475   3rd Qu.:3.30   3rd Qu.:4.900   3rd Qu.:1.80                  
#>  Max.   :7.900   Max.   :4.40   Max.   :6.400   Max.   :2.30                  
#>  NAs    :50      NAs    :50     NAs    :50      NAs    :50                    

# 50 more points and the function will be recomputed
update(s, stream, 50)
get_model(s)
#>   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
#>  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
#>  1st Qu.:5.175   1st Qu.:2.775   1st Qu.:1.600   1st Qu.:0.300  
#>  Median :5.750   Median :3.000   Median :4.400   Median :1.350  
#>  Mean   :5.830   Mean   :3.043   Mean   :3.783   Mean   :1.213  
#>  3rd Qu.:6.400   3rd Qu.:3.325   3rd Qu.:5.100   3rd Qu.:1.800  
#>  Max.   :7.900   Max.   :4.400   Max.   :6.700   Max.   :2.500  
#>        Species  
#>  setosa    :31  
#>  versicolor:36  
#>  virginica :33  
#>                 
#>                 
#>                 


## Example 2: Use classifier on the sliding window
reset_stream(stream)

# rpart, like most models in R, already have a formula interface that uses a
# data parameter. We can use these types of models directly
library(rpart)
cl <- DST_SlidingWindow(
  rpart, formula = Species ~ Petal.Length + Petal.Width,
  window = 100, rebuild = 50)
cl
#> Data Stream Task: Call function on a Sliding Window
#> Function: rpart 
#> Class: DST_SlidingWindow, DST 

# update window with 50 points so the model is built
update(cl, stream, 50)
get_model(cl)
#> n=50 (50 observations deleted due to missingness)
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 50 30 versicolor (0.32000000 0.40000000 0.28000000)  
#>   2) Petal.Length< 2.45 16  0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) Petal.Length>=2.45 34 14 versicolor (0.00000000 0.58823529 0.41176471)  
#>     6) Petal.Width< 1.65 19  0 versicolor (0.00000000 1.00000000 0.00000000) *
#>     7) Petal.Width>=1.65 15  1 virginica (0.00000000 0.06666667 0.93333333) *

# update with 40 more points and force the function to be recomputed even though it would take
#  50 points to automatically rebuild.
update(cl, stream, 40, rebuild = TRUE)
get_model(cl)
#> n=90 (10 observations deleted due to missingness)
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 90 56 versicolor (0.30000000 0.37777778 0.32222222)  
#>   2) Petal.Length< 2.45 27  0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) Petal.Length>=2.45 63 29 versicolor (0.00000000 0.53968254 0.46031746)  
#>     6) Petal.Width< 1.75 36  3 versicolor (0.00000000 0.91666667 0.08333333) *
#>     7) Petal.Width>=1.75 27  1 virginica (0.00000000 0.03703704 0.96296296) *

# rpart supports predict, so we can use it directly with the DST_SlidingWindow
new_points <- get_points(stream, n = 5)
predict(cl, new_points, type = "class")
#>         26        104         64        116        120 
#>     setosa  virginica versicolor  virginica versicolor 
#> Levels: setosa versicolor virginica

## Example 3: Regression using a sliding window
reset_stream(stream)

## lm can be directly used
reg <- DST_SlidingWindow(
  lm, formula = Sepal.Length ~ Petal.Width + Petal.Length,
  window = 100, rebuild = 50)
reg
#> Data Stream Task: Call function on a Sliding Window
#> Function: lm 
#> Class: DST_SlidingWindow, DST 

update(reg, stream, 100)
get_model(reg)
#> 
#> Call:
#> (function (formula, data, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> {
#>     ret.x <- x
#>     ret.y <- y
#>     cl <- match.call()
#>     mf <- match.call(expand.dots = FALSE)
#>     m <- match(c("formula", "data", "subset", "weights", "na.action", 
#>         "offset"), names(mf), 0L)
#>     mf <- mf[c(1L, m)]
#>     mf$drop.unused.levels <- TRUE
#>     mf[[1L]] <- quote(stats::model.frame)
#>     mf <- eval(mf, parent.frame())
#>     if (method == "model.frame") 
#>         return(mf)
#>     else if (method != "qr") 
#>         warning(gettextf("method = '%s' is not supported. Using 'qr'", 
#>             method), domain = NA)
#>     mt <- attr(mf, "terms")
#>     y <- model.response(mf, "numeric")
#>     w <- as.vector(model.weights(mf))
#>     if (!is.null(w) && !is.numeric(w)) 
#>         stop("'weights' must be a numeric vector")
#>     offset <- model.offset(mf)
#>     mlm <- is.matrix(y)
#>     ny <- if (mlm) 
#>         nrow(y)
#>     else length(y)
#>     if (!is.null(offset)) {
#>         if (!mlm) 
#>             offset <- as.vector(offset)
#>         if (NROW(offset) != ny) 
#>             stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
#>                 NROW(offset), ny), domain = NA)
#>     }
#>     if (is.empty.model(mt)) {
#>         x <- NULL
#>         z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
#>             ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
#>             y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
#>             0) else ny)
#>         if (!is.null(offset)) {
#>             z$fitted.values <- offset
#>             z$residuals <- y - offset
#>         }
#>     }
#>     else {
#>         x <- model.matrix(mt, mf, contrasts)
#>         z <- if (is.null(w)) 
#>             lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
#>                 ...)
#>         else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
#>             ...)
#>     }
#>     class(z) <- c(if (mlm) "mlm", "lm")
#>     z$na.action <- attr(mf, "na.action")
#>     z$offset <- offset
#>     z$contrasts <- attr(x, "contrasts")
#>     z$xlevels <- .getXlevels(mt, mf)
#>     z$call <- cl
#>     z$terms <- mt
#>     if (model) 
#>         z$model <- mf
#>     if (ret.x) 
#>         z$x <- x
#>     if (ret.y) 
#>         z$y <- y
#>     if (!qr) 
#>         z$qr <- NULL
#>     z
#> })(formula = Sepal.Length ~ Petal.Width + Petal.Length, data = structure(list(
#>     Sepal.Length = c(5.8, 6, 6.3, 4.9, 7.9, 4.4, 6.4, 7.3, 4.8, 
#>     5.5, 7.4, 6.5, 5, 4.6, 6.1, 4.6, 5.9, 6.4, 5.7, 5.7, 5.2, 
#>     5, 5.3, 6.6, 5.5, 6.4, 5.6, 6.9, 5.4, 7, 6.7, 5.5, 5.6, 6.2, 
#>     6.5, 4.9, 4.4, 5.5, 5, 4.9, 5.4, 5.1, 6.5, 6.8, 4.9, 6.6, 
#>     6.7, 5.9, 6.1, 5.5, 5.7, 6.3, 5.6, 4.3, 5.1, 6.7, 6, 5.7, 
#>     5, 7.7, 5.7, 5.9, 5.8, 6.3, 7.2, 7.1, 6.8, 4.9, 5.2, 6.3, 
#>     5.2, 6, 5.2, 5.1, 5.8, 4.8, 6.3, 5, 6.7, 5, 7.2, 7.2, 6.3, 
#>     6.2, 5.7, 5.5, 6.8, 5.8, 5.4, 5, 5, 6.3, 6.1, 6.4, 6, 5.7, 
#>     4.7, 5.1, 5.8, 5.6), Sepal.Width = c(2.7, 3.4, 2.5, 3, 3.8, 
#>     2.9, 3.2, 2.9, 3.4, 4.2, 2.8, 3.2, 3.5, 3.6, 2.8, 3.2, 3, 
#>     2.7, 4.4, 2.9, 3.5, 3.3, 3.7, 2.9, 2.3, 2.8, 2.5, 3.1, 3.4, 
#>     3.2, 3.1, 2.6, 3, 2.2, 2.8, 2.5, 3, 3.5, 2.3, 3.6, 3, 2.5, 
#>     3, 3, 3.1, 3, 3.3, 3.2, 3, 2.5, 2.5, 3.3, 2.8, 3, 3.8, 3.1, 
#>     2.7, 2.6, 3.4, 3.8, 2.8, 3, 2.7, 2.8, 3.2, 3, 2.8, 2.4, 2.7, 
#>     2.3, 3.4, 2.9, 4.1, 3.5, 4, 3, 2.7, 2, 3, 3.5, 3, 3.6, 2.5, 
#>     2.8, 2.8, 2.4, 3.2, 2.8, 3.4, 3.6, 3, 2.9, 2.9, 3.2, 2.2, 
#>     3.8, 3.2, 3.8, 2.7, 2.7), Petal.Length = c(5.1, 4.5, 4.9, 
#>     1.4, 6.4, 1.4, 4.5, 6.3, 1.9, 1.4, 6.1, 5.1, 1.3, 1, 4.7, 
#>     1.4, 5.1, 5.3, 1.5, 4.2, 1.5, 1.4, 1.5, 4.6, 4, 5.6, 3.9, 
#>     5.1, 1.7, 4.7, 4.4, 4.4, 4.1, 4.5, 4.6, 4.5, 1.3, 1.3, 3.3, 
#>     1.4, 4.5, 3, 5.2, 5.5, 1.5, 4.4, 5.7, 4.8, 4.9, 4, 5, 6, 
#>     4.9, 1.1, 1.5, 5.6, 5.1, 3.5, 1.6, 6.7, 4.5, 4.2, 4.1, 5.1, 
#>     6, 5.9, 4.8, 3.3, 3.9, 4.4, 1.4, 4.5, 1.5, 1.4, 1.2, 1.4, 
#>     4.9, 3.5, 5, 1.6, 5.8, 6.1, 5, 4.8, 4.1, 3.8, 5.9, 5.1, 1.5, 
#>     1.4, 1.6, 5.6, 4.7, 5.3, 5, 1.7, 1.3, 1.9, 5.1, 4.2), Petal.Width = c(1.9, 
#>     1.6, 1.5, 0.2, 2, 0.2, 1.5, 1.8, 0.2, 0.2, 1.9, 2, 0.3, 0.2, 
#>     1.2, 0.2, 1.8, 1.9, 0.4, 1.3, 0.2, 0.2, 0.2, 1.3, 1.3, 2.1, 
#>     1.1, 2.3, 0.2, 1.4, 1.4, 1.2, 1.3, 1.5, 1.5, 1.7, 0.2, 0.2, 
#>     1, 0.1, 1.5, 1.1, 2, 2.1, 0.2, 1.4, 2.1, 1.8, 1.8, 1.3, 2, 
#>     2.5, 2, 0.1, 0.3, 2.4, 1.6, 1, 0.4, 2.2, 1.3, 1.5, 1, 1.5, 
#>     1.8, 2.1, 1.4, 1, 1.4, 1.3, 0.2, 1.5, 0.1, 0.3, 0.2, 0.3, 
#>     1.8, 1, 1.7, 0.6, 1.6, 2.5, 1.9, 1.8, 1.3, 1.1, 2.3, 2.4, 
#>     0.4, 0.2, 0.2, 1.8, 1.4, 2.3, 1.5, 0.3, 0.2, 0.4, 1.9, 1.3
#>     ), Species = structure(c(3L, 2L, 2L, 1L, 3L, 1L, 2L, 3L, 
#>     1L, 1L, 3L, 3L, 1L, 1L, 2L, 1L, 3L, 3L, 1L, 2L, 1L, 1L, 1L, 
#>     2L, 2L, 3L, 2L, 3L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 1L, 1L, 
#>     2L, 1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L, 2L, 3L, 2L, 3L, 3L, 3L, 
#>     1L, 1L, 3L, 2L, 2L, 1L, 3L, 2L, 2L, 2L, 3L, 3L, 3L, 2L, 2L, 
#>     2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 3L, 2L, 2L, 1L, 3L, 3L, 3L, 
#>     3L, 2L, 2L, 3L, 3L, 1L, 1L, 1L, 3L, 2L, 3L, 3L, 1L, 1L, 1L, 
#>     3L, 2L), levels = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = c(NA, 
#> -100L), class = "data.frame"))
#> 
#> Coefficients:
#>  (Intercept)   Petal.Width  Petal.Length  
#>       4.1882       -0.4697        0.5846  
#> 

# lm supports predict, so we can use it directly with the DST_SlidingWindow
new_points <- get_points(stream, n = 5)
predict(reg, new_points)
#>       98       42       75      135        4 
#> 6.091381 4.807258 6.091381 6.804412 4.971155 
```
