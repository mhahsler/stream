library("testthat")
setwd(tempdir())

### read and reload write some DSCs (see if Cpp serialization works)
set.seed(0)
stream <- DSD_Gaussians(k = 3, noise = 0.05)

######################################################################
context("DSC_DBSTREAM")

# create clusterer with r = 0.05
dbstream <- DSC_DBSTREAM(r = .05)
update(dbstream, stream, 10000)
dbstream

saveDSC(dbstream, file="dbstream.Rds")
db <- readDSC("dbstream.Rds")
db

expect_equal(dbstream$RObj$micro, db$RObj$micro)
expect_equal(dbstream$macro, db$macro)

# cleanup
unlink("dbstream.Rds")

######################################################################
context("DSC_TwoStage")

dbstream <- DSC_TwoStage(micro=DSC_DBSTREAM(r = .05),
			  macro = DSC_Kmeans(k=3))
update(dbstream, stream, 10000)
dbstream

saveDSC(dbstream, file="dbstream.Rds")
db <- readDSC("dbstream.Rds")
db

expect_equal(dbstream$RObj$micro, db$RObj$micro)
expect_equal(dbstream$macro, db$macro)


######################################################################
context("DSC_DSTREAM")

dstream <- DSC_DStream(grid = .1)
update(dstream, stream, 10000)
dstream

saveDSC(dstream, file="dstream.Rds")
ds <- readDSC("dstream.Rds")
ds

expect_equal(dstream$RObj$micro, ds$RObj$micro)
expect_equal(dstream$macro, ds$macro)

# cleanup
unlink(c("dbstream.Rds", "dstream.Rds"))

