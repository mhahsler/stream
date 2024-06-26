test_that("DSF_Scale", {
  stream <- DSD_Gaussians(k = 3, d = 2) %>% DSD_Memory(n = 100)

  scaledStream <- stream %>%  DSF_Scale(n = 100)

  reset_stream(scaledStream)
  get_points(stream, n = 5)
  reset_stream(scaledStream)
  get_points(scaledStream, n = 5)

  reset_stream(scaledStream)
  expect_equal(round(colMeans(get_points(scaledStream, n = 100, info = FALSE)), digits = getOption("digits")),
    c(X1 = 0, X2 = 0))

  reset_stream(scaledStream)
  expect_equal(apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd), c(X1 = 1, X2 = 1))
})
