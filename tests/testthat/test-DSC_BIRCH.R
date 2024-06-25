test_that("DSC_BIRCH", {
  ### read and reload write some DSCs (see if Cpp serialization works)
  set.seed(0)
  stream <- DSD_Gaussians(k = 3, noise = 0.05)

  ######################################################################

  # create clusterer with r = 0.05
  BIRCH <- DSC_BIRCH(threshold = .1,
    branching = 8,
    maxLeaf = 20)
  update(BIRCH, stream, n = 10000)

  BIRCH

  ## saveDSC is not implemented!
  #saveDSC(BIRCH, file="BIRCH.Rds")
  #db <- readDSC("BIRCH.Rds")
  #
  #expect_equal(BIRCH$RObj$micro, db$RObj$micro)
  #expect_equal(BIRCH$macro, db$macro)
  #
  #unlink("BIRCH.Rds")
})
