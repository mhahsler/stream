library("testthat")
library("stream")

short_desc <- function(x)
  strsplit(description(x), " ")[[1L]][1L]

set.seed(0)
stream <-
  DSD_Gaussians(d = 4, k = 3, noise = 0.05) %>% DSD_Memory(n = 1500)

f <- ~ X1 + X3

algorithms <- list(
  DSC_DBSTREAM(f, r = .1),
  DSC_DStream(
    f,
    gridsize = .1,
    Cm = 1,
    gaptime = 100
  ),
  DSC_evoStream(f, .45),
  DSC_Reachability(f, epsilon = .1),
  DSC_Hierarchical(f, h = .1),
  DSC_Kmeans(f, k = 3),
  DSC_DBSCAN(f, eps = .05),
  DSC_BICO(f, k = 3),
  DSC_BIRCH(
    f,
    threshold = .1,
    branching = 8,
    maxLeaf = 20
  ),
  DSC_EA(f, k = 3, generations = 10)
  # DSC_Sample(f,k = 10),
  #  DSC_Window(f, horizon = 10)
)
names(algorithms) <- sapply(algorithms, short_desc)

context("DSC update")

up <- lapply(
  algorithms,
  FUN = function(a) {
    if (interactive())
      cat(paste("update:", short_desc(a)))
    reset_stream(stream)
    update(a, stream, n = 500L)
    if (interactive())
      cat(paste(" - clusters: ", nclusters(a)), "\n")
    expect_equal(colnames(get_centers(a)), c("X1", "X3"))
  }
)

### Add: check the result
if (interactive()) {
  print(algorithms)
  str(up)
}

