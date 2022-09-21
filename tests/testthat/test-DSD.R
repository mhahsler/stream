library(stream)
library("testthat")
setwd(tempdir())

df <- data.frame(
  x = runif(100),
  y = runif(100),
  .class = sample(1:3, 100, replace = TRUE)
)


############################################################################
context("DSD_Memory")

stream <- DSD_Memory(df)
reset_stream(stream)
points <- get_points(stream, n = 10, info = TRUE)
expect_equal(points, df[1:10, ])

### returned all 100 points
reset_stream(stream)
points <- get_points(stream, n = 100)
expect_equal(nrow(points), 100)
expect_equal(ncol(points), ncol(df))

### ask for too many
reset_stream(stream)
expect_warning(points <- get_points(stream, n = 101))
expect_equal(nrow(points), 100)


### no more points available
expect_warning(points <-
    get_points(stream, n = 500,))
expect_equal(nrow(points), 0)
expect_equal(ncol(points), ncol(df))

### test stop
stream <- DSD_Memory(df, outofpoints = "stop")
expect_error(points <- get_points(stream, n = 101))

##########################################################################
context("DSD_ReadCSV")

write_stream(
  DSD_Memory(df),
  file = "test.stream",
  header = TRUE,
  info = TRUE,
  n = 100
)

stream <- DSD_ReadCSV("test.stream", header = TRUE)
stream

reset_stream(stream)
points <- get_points(stream, n = 10, info = TRUE)
expect_equal(points, df[1:10, ])
expect_equal(ncol(points), ncol(df))

reset_stream(stream)
points <- get_points(stream, n = 100)
expect_equivalent(nrow(points), 100L)
expect_equal(ncol(points), ncol(df))


### check that a failed request does not take any points.
reset_stream(stream)
expect_warning(points <- get_points(stream, n = 101))
expect_equivalent(nrow(points), 100L)

expect_warning(points <- get_points(stream, n = 1))
expect_equivalent(nrow(points), 0L)

close_stream(stream)
stream

stream <-
  DSD_ReadCSV("test.stream", header = TRUE, outofpoints = "stop")
expect_error(points <-
    get_points(stream, n = 101))

close_stream(stream)
#unlink("test.stream")

########################################################################
context("DSD_ReadDB")

### NOTE: reset_stream is not implemented for DB connections

library("RSQLite")
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "gaussians", df)

### prepare a query result set
res <- dbSendQuery(con, "SELECT x, y, `.class` FROM gaussians")
stream <- DSD_ReadDB(res, k = 3)

### no reset for this
expect_error(reset_stream(stream))

points <- get_points(stream, n = 10)
expect_equivalent(nrow(points), 10L)
expect_equal(ncol(points), ncol(df))

expect_warning(points <- get_points(stream, n = 101))
expect_equivalent(nrow(points), 90L)

close_stream(stream, disconnect = FALSE)

###
res <- dbSendQuery(con, "SELECT x, y, `.class` FROM gaussians")
stream <- DSD_ReadDB(res, k = 3, outofpoints = "stop")

expect_error(points <-
    get_points(stream, n = 101))

close_stream(stream, disconnect = FALSE)

## Check general interface
dsd_inf <- list(
  DSD_BarsAndGaussians(),
  DSD_Benchmark(),
  DSD_Cubes(),
  DSD_Gaussians(),
  DSD_mlbenchGenerator("cassini"),
  DSD_Target(),
  DSD_UniformNoise()
)

res <- dbSendQuery(con, "SELECT x, y, `.class` FROM gaussians")

dsd_finite <- list(DSD_Memory(df),
  #DSD_Mixture,
  DSD_mlbenchData("PimaIndiansDiabetes"),
  DSD_ReadDB(res, k = 3),
  DSD_ReadStream("test.stream", header = TRUE)
  #DSD_ReadCSV,
)

  for (dsd in c(dsd_inf, dsd_finite)) {
    r <- get_points(dsd, n = 0)
    expect_equal(nrow(r), 0L)
    expect_true(inherits(r, "data.frame"))
    expect_false(!length(grep("^\\.", colnames(r))))
    r <- get_points(dsd, n = 0, info = FALSE)
    expect_equal(nrow(r), 0L)
    expect_true(inherits(r, "data.frame"))
    expect_true(!length(grep("^\\.", colnames(r))))

    expect_equal(colnames(get_points(dsd, n = 0)), colnames(get_points(dsd, n = 1)))

    r <- get_points(dsd, n = 1)
    expect_equal(nrow(r), 1L)
    expect_true(inherits(r, "data.frame"))
    expect_false(!length(grep("^\\.", colnames(r))))
    r <- get_points(dsd, n = 1, info = FALSE)
    expect_equal(nrow(r), 1L)
    expect_true(inherits(r, "data.frame"))
    expect_true(!length(grep("^\\.", colnames(r))))

    r <- get_points(dsd, n = 10)
    expect_equal(nrow(r), 10L)
    expect_true(inherits(r, "data.frame"))
    expect_false(!length(grep("^\\.", colnames(r))))
    r <- get_points(dsd, n = 10, info = FALSE)
    expect_equal(nrow(r), 10L)
    expect_true(inherits(r, "data.frame"))
    expect_true(!length(grep("^\\.", colnames(r))))
  }

  for (dsd in dsd_inf) {
    # error
    expect_error(get_points(dsd, n = -1), "not allowed")
    expect_error(get_points(dsd, n = -1, info = FALSE), "not allowed")
  }

  for (dsd in dsd_finite) {
    if (!inherits(dsd, "DSD_ReadDB"))
      reset_stream(dsd)
    get_points(dsd, n = -1)
    expect_equal(get_points(dsd, n = -1), get_points(dsd, n = 0))
  }

### cleanup
suppressWarnings(lapply(dsd_finite, close_stream))

unlink("test.stream")
