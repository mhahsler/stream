library("stream")
library("testthat")

context("DSC_SHC")
set.seed(0)

ds <- stream::DSD_Gaussians(k=10,outliers=10,separation_type="Mahalanobis",
                            separation=4,space_limit=c(0,150),variance_limit=8,
                            outlier_options=list(outlier_horizon=20000))
c <- DSC_SHC.behavioral(2, SHCAgglomerationType$AggresiveAgglomeration, SHCDriftType$NoDrift, 0, sigmaIndex = TRUE,
                        sigmaIndexNeighborhood = 2)
c$RObj$setPseudoOfflineCounter(500)
r <- evaluate_with_callbacks(c, ds, n=20000, measure = c("cRand","queryTime","updateTime",
                                                      "processTime","nodeCount",
                                                      "computationCostReduction", "outlierjaccard"),
                             type="macro", callbacks=list(shc=SHCEvalCallback()),
                             single_pass_update=T, use_outliers=T)


expect_gt(r$cRand,0.9)
expect_gt(r$OutlierJaccard, 0.9)
