test_that("write_stream", {
  stream <- DSD_Gaussians(k = 3, d = 5)
  tf <- tempfile()
  write_stream(stream, file = tf, n = 10, header = TRUE, info = TRUE)

  l <- readLines(tf)
  expect_length(l, 10L + 1L)

  dsd <- DSD_ReadStream(tf, header = TRUE)

  reset_stream(dsd)
  p <- get_points(dsd, n = -1)

  expect_identical(dim(p), dim(read.csv(text = l)))

  close_stream(dsd)
  unlink(tf)

  # n = -1
  write_stream(DSD_Memory(stream, 5), file = tf, n = -1, header = TRUE, info = TRUE)
  l <- readLines(tf)
  expect_length(l, 5L + 1L)
  unlink(tf)

  # n = 0
  write_stream(stream, file = tf, n = 0, header = TRUE, info = TRUE)
  l <- readLines(tf)
  expect_length(l, 1L)
  unlink(tf)
})

test_that("DST_WriteStream", {
  stream <- DSD_Gaussians(k = 3, d = 5)
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writer <- DST_WriteStream(tf)
  writer

  update(writer, stream, n = 0)
  l <- readLines(tf)
  expect_length(l, 0L)

  update(writer, stream, n = 5)
  l <- readLines(tf)
  expect_length(l, 5L)

  expect_error(DST_WriteStream(tf, header = TRUE))
  w2 <- DST_WriteStream(tf, append = TRUE)
  update(w2, stream, n = 10)
  l <- readLines(tf)
  expect_length(l, 15L)

  ## cleanup
  close_stream(writer)
  close_stream(w2)
})
