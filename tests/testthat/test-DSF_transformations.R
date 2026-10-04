test_that("DSF_Downsample returns every nth point", {
  source <- DSD_Memory(data.frame(X1 = 1:6, X2 = 11:16))
  sampled <- get_points(DSF_Downsample(source, factor = 2), n = 3, info = FALSE)
  sampled$weight <- NULL
  rownames(sampled) <- NULL

  expect_equal(sampled, data.frame(X1 = c(1L, 3L, 5L), X2 = c(11L, 13L, 15L)))
})

test_that("DSF_FeatureSelection keeps requested features", {
  source <- DSD_Memory(data.frame(X1 = 1:3, X2 = 11:13, X3 = 21:23))
  selected <- get_points(
    DSF_FeatureSelection(source, features = c("X1", "X3")),
    n = 2,
    info = FALSE
  )

  expect_equal(selected, data.frame(X1 = 1:2, X3 = 21:22))
})

test_that("DSF_Convolve applies a one point identity kernel", {
  source <- DSD_Memory(data.frame(X1 = c(2, 4, 8)))
  filtered <- get_points(
    DSF_Convolve(source, kernel = 1),
    n = 3,
    info = FALSE
  )
  filtered$weight <- NULL

  expect_equal(filtered, data.frame(X1 = c(2, 4, 8)))
})

test_that("DSF_ExponentialMA carries its running state across updates", {
  source <- DSD_Memory(data.frame(X1 = c(0, 2, 4)))
  smoothed <- DSF_ExponentialMA(source, alpha = 0.5)

  first <- update(smoothed, n = 2, info = FALSE)
  second <- update(smoothed, n = 1, info = FALSE)

  expect_equal(first$X1, c(0, 1))
  expect_equal(second$X1, 2.5)
})

test_that("DSF_Scale uses supplied factors and estimates them from consumed points", {
  source <- DSD_Memory(data.frame(X1 = c(1, 3, 5), X2 = c(10, 20, 30)))
  scaled <- get_points(
    DSF_Scale(source, dim = c("X1", "X2"), center = c(1, 10), scale = c(2, 10)),
    n = 2,
    info = FALSE
  )

  expect_equal(scaled, data.frame(X1 = c(0, 1), X2 = c(0, 1)))

  source <- DSD_Memory(data.frame(X1 = 1:5))
  DSF_Scale(source, n = 2)
  expect_equal(get_points(source, n = 1, info = FALSE)$X1, 3)
})
