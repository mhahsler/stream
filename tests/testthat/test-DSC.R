library("testthat")
library("stream")

short_desc <- function(x)
  strsplit(description(x), " ")[[1L]][1L]

set.seed(0)
stream <-
  DSD_Gaussians(d = 2, k = 3, noise = 0.05) %>% DSD_Memory(n = 1500)

algorithms <- list(
  DSC_DBSTREAM(r = .1),
  DSC_DStream(
    gridsize = .1,
    Cm = 1,
    gaptime = 100
  ),
  DSC_evoStream(r = .45),
  DSC_Kmeans(k = 3),
  DSC_Hierarchical(k = 3),
  DSC_Reachability(epsilon = .1),
  DSC_DBSCAN(eps = .05),
  DSC_BICO(k = 3),
  DSC_BIRCH(
    threshold = .1,
    branching = 8,
    maxLeaf = 20
  ),
  DSC_EA(k = 3, generations = 10),
  DSC_Sample(k = 10),
  DSC_Window(horizon = 10)
)
names(algorithms) <- sapply(algorithms, short_desc)

context("DSC update")

up <- lapply(
  algorithms,
  FUN = function(a) {
    if (interactive())
      cat(paste("update:", short_desc(a)), "\n")
    reset_stream(stream)
    u <- update(a, stream, n = 1000L, return = "assignment")
    expect_true(is.null(u) ||
        (is.data.frame(u) &&
            nrow(u) == 1000L && !is.null(u[[".class"]])))
    if (interactive())
      str(u)

    u
  }
)

### Add: check the result
if (interactive()) {
  print(algorithms)
  str(up)
}

context("DSC update -1 and data.frame")

dsc <- DSC_DBSTREAM(r = .1)
reset_stream(stream)

# n is ignored with a warning for update with a data.frame
expect_warning(ass <- update(dsc, get_points(stream, n = 10), n = 5, return = "assignment"))
expect_equal(nrow(ass), 10L)

# update with all points
ass <- update(dsc, get_points(stream, n = 10), return = "assignment")
expect_equal(nrow(ass), 10L)

# test 0
reset_stream(stream)
ass <- update(dsc, stream, n = 0, return = "assignment")
expect_equal(nrow(ass), 0L)

# test -1
reset_stream(stream)
ass <- update(dsc, stream, n = -1, return = "assignment")
expect_equal(nrow(ass), 1500L)



context("DSC evaluate")

ms <-
  c(
    "numMicroClusters",
    "numMacroClusters",
    "noiseActual",
    "noisePredicted",
    "purity",
    "CRand"
  )

evals <- sapply(
  algorithms,
  FUN = function(a) {
    if (interactive())
      cat(paste("evaluate:", short_desc(a)), "\n")
    reset_stream(stream, pos = 1001)
    e <- evaluate_static(a,
      stream,
      measure = ms,
      type = "micro",
      n = 500)
    expect_equal(length(e), length(ms))
    e
  }
)

if (interactive()) {
  print(round(evals, 2))
}

