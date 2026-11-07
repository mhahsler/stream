test_that("evaluate_stream uses the same measure names as evaluate_static", {
  set.seed(1000)
  stream <- DSD_Memory(DSD_Benchmark(1), n = 600)
  dbstream <- DSC_DBSTREAM(r = 0.05, lambda = 0.01)

  # measure names are matched case-insensitively
  ev <- evaluate_stream(dbstream, stream, measure = c("crand", "purity"),
    n = 600, horizon = 100, type = "macro", assign = "micro")

  expect_identical(colnames(ev), c("points", "cRand", "purity"))
  expect_equal(nrow(ev), 6L)

  reset_stream(stream)
  es <- evaluate_static(dbstream, stream, measure = c("crand", "purity"),
    n = 100, type = "macro", assign = "micro")
  expect_identical(colnames(ev)[-1L], names(es))
})

test_that("evaluate_stream expands partial measure names", {
  set.seed(1000)
  stream <- DSD_Memory(DSD_Benchmark(1), n = 300)
  ev <- evaluate_stream(DSC_DBSTREAM(r = 0.05), stream,
    measure = "numMicro", n = 300, horizon = 100)
  expect_identical(colnames(ev), c("points", "numMicroClusters"))
})

test_that("print.stream_eval does not print attributes", {
  set.seed(1000)
  stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
  dstream <- DSC_DStream(gridsize = 0.1)
  update(dstream, stream, n = 500)
  es <- evaluate_static(dstream, stream, n = 100,
    measure = c("purity", "cRand"), type = "micro")

  out <- capture.output(res <- print(es))
  expect_false(any(grepl("attr\\(", out)))
  expect_true(any(grepl("micro-clusters", out)))
  expect_true(any(grepl("purity", out)))
  # print returns its argument invisibly
  expect_identical(res, es)
})
