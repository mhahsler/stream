# Source: https://github.com/r-lib/backports/blob/main/R/deparse1.R
# can be removed when R 4.0.0 is used
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}
