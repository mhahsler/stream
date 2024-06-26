expect_equal_dsc <- function (x, y) {
  for (type in c("micro", "macro")) {
    expect_equal(
      get_centers(x, type = type),
      get_centers(y, type = type)
    )
    expect_equal(
      get_weights(x, type = type),
      get_weights(y, type = type)
    )
  }
}


######################################################################
### read and reload write some DSCs (see if Cpp serialization works)

set.seed(0)
stream <- DSD_Gaussians(k = 3, noise = 0.05)

test_that("read/write DSC_DBSTREAM", {
  # create clusterer with r = 0.05
  dsc <- DSC_DBSTREAM(r = .05)
  update(dsc, stream, 10000)
  dsc

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveDSC(dsc, file = tf)
  dsc_read <- readDSC(tf)
  dsc_read

  expect_equal_dsc(dsc, dsc_read)
})

test_that("read/write DSC_TwoStage", {
  dsc <- DSC_TwoStage(micro = DSC_DBSTREAM(r = .05),
    macro = DSC_Kmeans(k = 3))
  update(dsc, stream, 10000)

  # this forces the the reclustering otherwise we may get two
  # different reclustering results
  get_centers(dsc, type = "macro")

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveDSC(dsc, file = tf)
  dsc_read <- readDSC(tf)
  dsc_read

  expect_equal_dsc(dsc, dsc_read)
  expect_equal_dsc(dsc$micro, dsc_read$micro)
  expect_equal_dsc(dsc$macro, dsc_read$macro)
})

test_that("read/write DSC_DSTREAM", {
  dsc <- DSC_DStream(grid = .1)
  update(dsc, stream, 10000)
  dsc

  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveDSC(dsc, file = tf)
  dsc_read <- readDSC(tf)
  dsc_read

  expect_equal_dsc(dsc, dsc_read)
})
