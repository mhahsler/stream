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

dstream <- DSC_DStream(grid = .1) 
update(dstream, stream, 10000)
dstream 

saveDSC(dstream, file="dstream.Rds")
db <- readDSC("dstream.Rds")
db

# cleanup
unlink(c("dbstream.Rds", "dstream.Rds"))





