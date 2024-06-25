stream1 <- DSD_Gaussians(k = 3, d = 2)
stream2 <- DSD_Gaussians(k = 1, d = 2)

rename <- function(x, names) {
  colnames(x) <- names
  x
}

test_that("DSF_Func with a DSD", {
  skip_if_not_installed("dplyr")
  # DSF with stream
  dsfs <- list(
    stream1 %>% DSF_Func(func = rename, names = c("x", "y")),
    stream1 %>% DSF_Convolve(kernel = filter_MA(5), dim = 1),
    stream1 %>% DSF_Downsample(factor = 1),
    stream1 %>% DSF_dplyr(
      func = dplyr::mutate(Xsum = X1 + X2)) %>% DSF_dplyr(func = dplyr::select(!X1)
    ),
    stream1 %>% DSF_ExponentialMA(alpha = .7),
    stream1 %>% DSF_Scale()
  )

  for (stream_dsf in dsfs) {
    if (interactive()) {
      print(stream_dsf)
      cat("\n")
    }

    ps <- get_points(stream_dsf, n = 5)
    ps$weight <- NULL
    expect_identical(dim(ps), c(5L, 3L))

    ps <- get_points(stream_dsf, n = 5, info = FALSE)
    ps$weight <- NULL
    expect_identical(dim(ps), c(5L, 2L))

    ps <- get_points(stream_dsf, n = 0)
    ps$weight <- NULL
    expect_identical(dim(ps), c(0L, 3L))

    ps <- update(stream_dsf, n = 5)
    ps$weight <- NULL
    expect_identical(nrow(ps), 5L)

    ps <- update(stream_dsf, dsd = stream2, n = 5)
    ps$weight <- NULL
    expect_identical(nrow(ps), 5L)
  }
})

test_that("DSF_Func without a DSD", {
  skip_if_not_installed("dplyr")

  expect_error(DSF_Scale())
  points <- get_points(stream1, n = 100, info = FALSE)
  center <- colMeans(points)
  scale <- apply(points, MARGIN = 2, sd)


  # DSF without stream
  dsfs <- list(
    DSF_Func(func = rename, names = c("x", "y")),
    DSF_Convolve(kernel = filter_MA(5), dim = 1),
    DSF_Downsample(factor = 1),
    DSF_dplyr(
      func = dplyr::mutate(Xsum = X1 + X2)) %>% DSF_dplyr(func = dplyr::select(!X1)
    ),
    DSF_ExponentialMA(alpha = .7),
    DSF_Scale(center = center, scale = scale, dim = c("X1", "X2"))
  )

  for (dsf in dsfs) {
    if (interactive()) {
      print(dsf)
      cat("\n")
    }

    # no stream
    expect_error(get_points(dsf, n = 5))

    # specify stream
    expect_identical(nrow(update(dsf, dsd = stream2, n = 5)), 5L)

    # use data.frame
    expect_identical(nrow(update(dsf, dsd = get_points(stream2, n = 5))), 5L)
  }


  # DSF Feature Selection
  stream <- DSD_Gaussians(k = 3, d = 3)

  stream_2features <- DSF_FeatureSelection(stream, features = c("X1", "X3"))
  p <- get_points(stream_2features, n = 3)
  expect_identical(dim(p), c(3L, 3L))

  p <- get_points(stream_2features, n = 3, info = FALSE)
  expect_identical(dim(p), c(3L, 2L))

  p <- get_points(stream_2features, n = 0)
  expect_identical(dim(p), c(0L, 3L))

  p <- get_points(stream_2features, n = 0, info = FALSE)
  expect_identical(dim(p), c(0L, 2L))

  stream_2features <- DSF_FeatureSelection(stream, features = c("X1", "X4"))
  expect_error(get_points(stream_2features, n = 3, info = FALSE))
})
