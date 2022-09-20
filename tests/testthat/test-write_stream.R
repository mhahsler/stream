library(stream)
library("testthat")
setwd(tempdir())

############################################################################
context("write_stream")

stream <- DSD_Gaussians(k = 3, d = 5)
write_stream(stream, file = "data.txt", n = 10, header = TRUE, info = TRUE)

l <- readLines("data.txt")
expect_equal(length(l), 10L + 1L)

dsd <- DSD_ReadStream("data.txt", header = TRUE)

reset_stream(dsd)
p <- get_points(dsd, n = -1)

expect_equal(dim(p), dim(read.csv(text = l)))

file.remove("data.txt")

# n = -1
write_stream(DSD_Memory(stream, 5), file = "data.txt", n = -1, header = TRUE, info = TRUE)

l <- readLines("data.txt")
expect_equal(length(l), 5L + 1L)

file.remove("data.txt")

# n = 0

write_stream(stream, file = "data.txt", n = 0, header = TRUE, info = TRUE)

l <- readLines("data.txt")
expect_equal(length(l), 1L)

file.remove("data.txt")

############################################################################

context("DST_WriteStream")

stream <- DSD_Gaussians(k = 3, d = 5)
writer <- DST_WriteStream("data.txt")
writer

update(writer, stream, n = 0)
l <- readLines("data.txt")
expect_equal(length(l), 0L)

update(writer, stream, n = 5)
l <- readLines("data.txt")
expect_equal(length(l), 5L)

expect_error(DST_WriteStream("data.txt", header = TRUE))
w2 <- DST_WriteStream("data.txt", append = TRUE)
update(w2, stream, n = 10)
l <- readLines("data.txt")
expect_equal(length(l), 15L)

## cleanup
close_stream(writer)
close_stream(w2)
file.remove("data.txt")

