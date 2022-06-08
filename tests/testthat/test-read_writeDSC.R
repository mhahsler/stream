library("testthat")
library("stream")

setwd(tempdir())

expect_equal_dsc_basics <- function (x,
  y,
  fields =
    c("centers",
      "weights",
      "micro",
      "macro",
      "CppObj",
      "microToMacro"))
  for (f in fields)
    expect_equal(x[[f]], y[[f]])

######################################################################
### read and reload write some DSCs (see if Cpp serialization works)

set.seed(0)
stream <- DSD_Gaussians(k = 3, noise = 0.05)

context("read/write DSC_DBSTREAM")

# create clusterer with r = 0.05
dsc <- DSC_DBSTREAM(r = .05)
update(dsc, stream, 10000)
dsc

saveDSC(dsc, file = "dsc.Rds")
dsc_read <- readDSC("dsc.Rds")
dsc_read

#expect_equal(dsc$RObj$micro, dsc_read$RObj$micro)
expect_equal_dsc_basics(dsc$RObj, dsc_read$RObj)

# this should work, but has an issue with comparing the class
#expect_equal(dsc$macro$RObj, dsc_read$macro$RObj)
expect_equal_dsc_basics(dsc$macro$RObj, dsc_read$macro$RObj)

# cleanup
unlink("dsc.Rds")

######################################################################
context("read/write DSC_TwoStage")

dsc <- DSC_TwoStage(micro = DSC_DBSTREAM(r = .05),
  macro = DSC_Kmeans(k = 3))
update(dsc, stream, 10000)
dsc

saveDSC(dsc, file = "dsc.Rds")
dsc_read <- readDSC("dsc.Rds")
dsc_read

#expect_equal(dsc$micro$RObj, dsc_read$micro$RObj)
expect_equal_dsc_basics(dsc$micro$RObj, dsc_read$micro$RObj)

#expect_equal(dsc$macro$RObj, dsc_read$macro$RObj)
expect_equal_dsc_basics(dsc$macro$RObj, dsc_read$macro$RObj)

unlink("dsc.Rds")

######################################################################
context("read/write DSC_DSTREAM")

dsc <- DSC_DStream(grid = .1)
update(dsc, stream, 10000)
dsc

saveDSC(dsc, file = "dsc.Rds")
dsc_read <- readDSC("dsc.Rds")
dsc_read

#expect_equal(dsc$micro$RObj, dsc_read$micro$RObj)
expect_equal_dsc_basics(dsc$micro$RObj, dsc_read$micro$RObj)

#expect_equal(dsc$macro$RObj, dsc_read$macro$RObj)
expect_equal_dsc_basics(dsc$macro$RObj, dsc_read$macro$RObj)

# cleanup
unlink(c("dsc.Rds"))
