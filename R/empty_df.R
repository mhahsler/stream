empty_df <- function(colnames) {
  as.data.frame(matrix(
    NA,
    nrow = 0,
    ncol = length(colnames),
    dimnames = list(row = NULL, col = colnames)
  ))
}
