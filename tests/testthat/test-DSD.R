df <- data.frame(
  x = runif(100),
  y = runif(100),
  .class = sample(1:3, 100, replace = TRUE)
)

test_that("DSD_Memory", {
  stream <- DSD_Memory(df)
  reset_stream(stream)
  points <- get_points(stream, n = 10, info = TRUE)
  expect_identical(points, df[1:10, ])

  ### returned all 100 points
  reset_stream(stream)
  points <- get_points(stream, n = 100)
  expect_identical(nrow(points), 100L)
  expect_identical(ncol(points), ncol(df))

  ### ask for too many
  reset_stream(stream)
  expect_warning(points <- get_points(stream, n = 101))
  expect_identical(nrow(points), 100L)


  ### no more points available
  expect_warning(points <-
      get_points(stream, n = 500,))
  expect_identical(nrow(points), 0L)
  expect_identical(ncol(points), ncol(df))

  ### test stop
  stream <- DSD_Memory(df, outofpoints = "stop")
  expect_error(points <- get_points(stream, n = 101))
})

test_that("DSD_ReadCSV", {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)

  write_stream(
    DSD_Memory(df),
    file = tf,
    header = TRUE,
    info = TRUE,
    n = 100
  )

  stream <- DSD_ReadCSV(tf, header = TRUE)
  stream

  reset_stream(stream)
  points <- get_points(stream, n = 10, info = TRUE)
  expect_equal(points, df[1:10, ])
  expect_identical(ncol(points), ncol(df))

  reset_stream(stream)
  points <- get_points(stream, n = 100)
  expect_identical(nrow(points), 100L)
  expect_identical(ncol(points), ncol(df))


  ### check that a failed request does not take any points.
  reset_stream(stream)
  expect_warning(points <- get_points(stream, n = 101))
  expect_identical(nrow(points), 100L)

  expect_warning(points <- get_points(stream, n = 1))
  expect_identical(nrow(points), 0L)

  close_stream(stream)
  stream

  stream <-
    DSD_ReadCSV(tf, header = TRUE, outofpoints = "stop")
  expect_error(points <-
      get_points(stream, n = 101))

  close_stream(stream)
})

test_that("DSD_ReadDB", {
  ### NOTE: reset_stream is not implemented for DB connections

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "gaussians", df)

  ### prepare a query result set
  res <- DBI::dbSendQuery(con, "SELECT x, y, `.class` FROM gaussians")
  stream <- DSD_ReadDB(res, k = 3)

  ### no reset for this
  expect_error(reset_stream(stream))

  points <- get_points(stream, n = 10)
  expect_identical(nrow(points), 10L)
  expect_identical(ncol(points), ncol(df))

  expect_warning(points <- get_points(stream, n = 101))
  expect_identical(nrow(points), 90L)

  close_stream(stream, disconnect = FALSE)

  ###
  res <- DBI::dbSendQuery(con, "SELECT x, y, `.class` FROM gaussians")
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

  res <- DBI::dbSendQuery(con, "SELECT x, y, `.class` FROM gaussians")

  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  write_stream(
    DSD_Memory(df),
    file = tf,
    header = TRUE,
    info = TRUE,
    n = 100
  )

  dsd_finite <- list(
    DSD_Memory(df),
    #DSD_Mixture,
    DSD_mlbenchData("PimaIndiansDiabetes"),
    DSD_ReadDB(res, k = 3),
    DSD_ReadStream(tf, header = TRUE)
    #DSD_ReadCSV,
  )

    for (dsd in c(dsd_inf, dsd_finite)) {
      r <- get_points(dsd, n = 0)
      expect_identical(nrow(r), 0L)
      expect_s3_class(r, "data.frame")
      expect_match(colnames(r), "^\\.", all = FALSE)
      r <- get_points(dsd, n = 0, info = FALSE)
      expect_identical(nrow(r), 0L)
      expect_s3_class(r, "data.frame")
      expect_no_match(colnames(r), "^\\.", all = FALSE)

      expect_equal(colnames(get_points(dsd, n = 0)), colnames(get_points(dsd, n = 1)))

      r <- get_points(dsd, n = 1)
      expect_identical(nrow(r), 1L)
      expect_s3_class(r, "data.frame")
      expect_match(colnames(r), "^\\.", all = FALSE)
      r <- get_points(dsd, n = 1, info = FALSE)
      expect_identical(nrow(r), 1L)
      expect_s3_class(r, "data.frame")
      expect_no_match(colnames(r), "^\\.", all = FALSE)

      r <- get_points(dsd, n = 10)
      expect_identical(nrow(r), 10L)
      expect_s3_class(r, "data.frame")
      expect_match(colnames(r), "^\\.", all = FALSE)
      r <- get_points(dsd, n = 10, info = FALSE)
      expect_identical(nrow(r), 10L)
      expect_s3_class(r, "data.frame")
      expect_no_match(colnames(r), "^\\.", all = FALSE)
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
})
