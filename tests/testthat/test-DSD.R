library("testthat")
setwd(tempdir())

df <- data.frame(x=runif(100), y=runif(100), 
  assignment=sample(1:3, 100, replace=TRUE))


############################################################################
context("DSD_Memory")

stream <- DSD_Memory(df[,1:2], class=df[,3])  
reset_stream(stream)
points <- get_points(stream, n=10)
expect_equal(nrow(points), 10) 

reset_stream(stream)
### returned all 100 points
expect_error(points <- get_points(stream, n=500))
expect_warning(points <- get_points(stream, n=500, outofpoints = "warn"))
expect_equal(nrow(points), 100) 

### no more points available
expect_error(points <- get_points(stream, n=500))
expect_warning(points <- get_points(stream, n=500, outofpoints = "warn"))
expect_equal(nrow(points), 0)

##########################################################################
context("DSD_ReadCSV")

write_stream(DSD_Memory(df[,1:2], class=df[,3]), file="test.stream", 
  n=100, class=TRUE)

stream <- DSD_ReadCSV("test.stream", class=3)  

reset_stream(stream)
points <- get_points(stream, n=10)
expect_equivalent(nrow(points), 10L) 

reset_stream(stream)
points <- get_points(stream, n=100)
expect_equivalent(nrow(points), 100L) 

reset_stream(stream)
points <- NA
expect_error(points <- get_points(stream, n=101))
points <- get_points(stream, n=100)
expect_error(points <- get_points(stream, n=1))

reset_stream(stream)
points <- NA
expect_warning(points <- get_points(stream, n=101, outofpoints = "warn"))
expect_equivalent(nrow(points), 100L) 

unlink("test.stream")

########################################################################
context("DSD_ReadDB")

library("RSQLite")
con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "gaussians", df)

### prepare a query result set  
res <- dbSendQuery(con, "SELECT x, y, assignment FROM gaussians")
stream <- DSD_ReadDB(res, k=3, class = 3)

### no reset for this
expect_error(reset_stream(stream)) 
points <- get_points(stream, n=10)
expect_equivalent(nrow(points), 10L) 
 
points <- NA
expect_error(points <- get_points(stream, n=101))
dbClearResult(res)

###
res <- dbSendQuery(con, "SELECT x, y, assignment FROM gaussians")
stream <- DSD_ReadDB(res, k=3, class = 3)

points <- NA
expect_warning(points <- get_points(stream, n=101, outofpoints = "warn"))
expect_equivalent(nrow(points), 100L) 
dbClearResult(res)
dbDisconnect(con)

