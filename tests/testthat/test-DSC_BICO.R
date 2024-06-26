test_that("DSC_BICO", {
  ### read and reload write some DSCs (see if Cpp serialization works)
  set.seed(0)
  stream <- DSD_Gaussians(k = 3, noise = 0.05)

  ######################################################################

  # create clusterer with r = 0.05
  BICO <- DSC_BICO(
    k = 3,
    p = 10,
    space = 100,
    iterations = 10
  )
  update(BICO, stream, n = 10000)

  ## saveDSC is not implemented!
  # saveDSC(BICO, file="BICO.Rds")
  # db <- readDSC("BICO.Rds")
  #
  # expect_equal(BICO$RObj$micro, db$RObj$micro)
  # expect_equal(BICO$macro, db$macro)
  #
  # unlink("BICO.Rds")
})
