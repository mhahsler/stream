# Package index

## Package overview

- [`stream`](http://michael.hahsler.net/stream/reference/stream-package.md)
  [`stream-package`](http://michael.hahsler.net/stream/reference/stream-package.md)
  : stream: Infrastructure for Data Stream Mining
- [`DST_Runner()`](http://michael.hahsler.net/stream/reference/stream_pipeline.md)
  : Create a Data Stream Pipeline

## Data stream sources

- [`DSD()`](http://michael.hahsler.net/stream/reference/DSD.md)
  [`DSD_R()`](http://michael.hahsler.net/stream/reference/DSD.md) : Data
  Stream Data Generator Base Classes
- [`DSD_Memory()`](http://michael.hahsler.net/stream/reference/DSD_Memory.md)
  : A Data Stream Interface for Data Stored in Memory
- [`DSD_Gaussians()`](http://michael.hahsler.net/stream/reference/DSD_Gaussians.md)
  : Mixture of Gaussians Data Stream Generator
- [`DSD_BarsAndGaussians()`](http://michael.hahsler.net/stream/reference/DSD_BarsAndGaussians.md)
  : Data Stream Generator for Bars and Gaussians
- [`DSD_Cubes()`](http://michael.hahsler.net/stream/reference/DSD_Cubes.md)
  : Static Cubes Data Stream Generator
- [`DSD_MG()`](http://michael.hahsler.net/stream/reference/DSD_MG.md)
  [`add_cluster()`](http://michael.hahsler.net/stream/reference/DSD_MG.md)
  [`get_clusters()`](http://michael.hahsler.net/stream/reference/DSD_MG.md)
  [`remove_cluster()`](http://michael.hahsler.net/stream/reference/DSD_MG.md)
  : DSD Moving Generator
- [`DSD_Mixture()`](http://michael.hahsler.net/stream/reference/DSD_Mixture.md)
  : Mixes Data Points from Several Streams into a Single Stream
- [`DSD_NULL()`](http://michael.hahsler.net/stream/reference/DSD_NULL.md)
  : Placeholder for a DSD Stream
- [`DSD_Target()`](http://michael.hahsler.net/stream/reference/DSD_Target.md)
  : Target Data Stream Generator
- [`DSD_UniformNoise()`](http://michael.hahsler.net/stream/reference/DSD_UniformNoise.md)
  : Uniform Noise Data Stream Generator
- [`DSD_Benchmark()`](http://michael.hahsler.net/stream/reference/DSD_Benchmark.md)
  : Data Stream Generator for Dynamic Data Stream Benchmarks
- [`DSD_mlbenchData()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchData.md)
  : Stream Interface for Data Sets From mlbench
- [`DSD_mlbenchGenerator()`](http://michael.hahsler.net/stream/reference/DSD_mlbenchGenerator.md)
  : mlbench Data Stream Generator
- [`DSD_ReadDB()`](http://michael.hahsler.net/stream/reference/DSD_ReadDB.md)
  [`close_stream(`*`<DSD_ReadDB>`*`)`](http://michael.hahsler.net/stream/reference/DSD_ReadDB.md)
  : Read a Data Stream from an open DB Query
- [`DSD_ReadStream()`](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md)
  [`DSD_ReadCSV()`](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md)
  [`close_stream(`*`<DSD_ReadStream>`*`)`](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md)
  [`close_stream(`*`<DSD_ReadCSV>`*`)`](http://michael.hahsler.net/stream/reference/DSD_ReadStream.md)
  : Read a Data Stream from a File or a Connection
- [`DSD_ScaleStream()`](http://michael.hahsler.net/stream/reference/DSD_ScaleStream.md)
  : Deprecated DSD_ScaleStream
- [`MGC()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`MGC_Function()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`MGC_Linear()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`keyframe()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`add_keyframe()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`get_keyframes()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`remove_keyframe()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`MGC_Noise()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`MGC_Random()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`Shape_Gaussian()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`Shape_Block()`](http://michael.hahsler.net/stream/reference/MGC.md)
  [`MGC_Static()`](http://michael.hahsler.net/stream/reference/MGC.md) :
  Moving Generator Cluster
- [`get_points()`](http://michael.hahsler.net/stream/reference/get_points.md)
  [`remove_info()`](http://michael.hahsler.net/stream/reference/get_points.md)
  : Get Points from a Data Stream Generator
- [`reset_stream()`](http://michael.hahsler.net/stream/reference/reset_stream.md)
  : Reset a Data Stream to its Beginning
- [`close_stream()`](http://michael.hahsler.net/stream/reference/close_stream.md)
  : Close a Data Stream

## Stream filters

- [`DSF()`](http://michael.hahsler.net/stream/reference/DSF.md)
  [`reset_stream(`*`<DSF>`*`)`](http://michael.hahsler.net/stream/reference/DSF.md)
  [`get_points(`*`<DSF>`*`)`](http://michael.hahsler.net/stream/reference/DSF.md)
  [`update(`*`<DSF>`*`)`](http://michael.hahsler.net/stream/reference/DSF.md)
  [`close_stream(`*`<DSF>`*`)`](http://michael.hahsler.net/stream/reference/DSF.md)
  : Data Stream Filter Base Classes
- [`DSF_Downsample()`](http://michael.hahsler.net/stream/reference/DSF_Downsample.md)
  : Downsample a Data Stream
- [`DSF_Convolve()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md)
  [`filter_MA()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md)
  [`filter_Hamming()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md)
  [`filter_diff()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md)
  [`filter_Sinc()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md)
  [`pow2()`](http://michael.hahsler.net/stream/reference/DSF_Convolve.md)
  : Apply a Filter to a Data Stream
- [`DSF_ExponentialMA()`](http://michael.hahsler.net/stream/reference/DSF_ExponentialMA.md)
  : Exponential Moving Average over a Data Stream
- [`DSF_FeatureSelection()`](http://michael.hahsler.net/stream/reference/DSF_FeatureSelection.md)
  : Select Features for a Data Stream
- [`DSF_Func()`](http://michael.hahsler.net/stream/reference/DSF_Func.md)
  : Apply a Function to Transform a Data Stream
- [`DSF_Scale()`](http://michael.hahsler.net/stream/reference/DSF_Scale.md)
  : Scale a Data Stream
- [`DSF_dplyr()`](http://michael.hahsler.net/stream/reference/DSF_dplyr.md)
  : Apply a dplyr Transformation to a Data Stream

## Stream tasks and operators

- [`DST()`](http://michael.hahsler.net/stream/reference/DST.md)
  [`description()`](http://michael.hahsler.net/stream/reference/DST.md)
  [`get_model()`](http://michael.hahsler.net/stream/reference/DST.md) :
  Conceptual Base Class for All Data Stream Mining Tasks
- [`DST_Multi()`](http://michael.hahsler.net/stream/reference/DST_Multi.md)
  : Apply Multiple Task to the Same Data Stream
- [`DST_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md)
  [`update(`*`<DST_SlidingWindow>`*`)`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md)
  [`predict(`*`<DST_SlidingWindow>`*`)`](http://michael.hahsler.net/stream/reference/DST_SlidingWindow.md)
  : DST_SlidingWindow – Call R Functions on a Sliding Window
- [`DST_WriteStream()`](http://michael.hahsler.net/stream/reference/DST_WriteStream.md)
  [`close_stream(`*`<DST_WriteStream>`*`)`](http://michael.hahsler.net/stream/reference/DST_WriteStream.md)
  : Task to Write a Stream to a File or a Connection
- [`DSFP()`](http://michael.hahsler.net/stream/reference/DSFP.md) :
  Abstract Class for Frequent Pattern Mining Algorithms for Data Streams
- [`DSAggregate()`](http://michael.hahsler.net/stream/reference/DSAggregate.md)
  [`update(`*`<DSAggregate>`*`)`](http://michael.hahsler.net/stream/reference/DSAggregate.md)
  [`get_points(`*`<DSAggregate>`*`)`](http://michael.hahsler.net/stream/reference/DSAggregate.md)
  [`get_weights(`*`<DSAggregate>`*`)`](http://michael.hahsler.net/stream/reference/DSAggregate.md)
  : Data Stream Aggregator Base Classes
- [`DSAggregate_Sample()`](http://michael.hahsler.net/stream/reference/DSAggregate_Sample.md)
  : Sampling from a Data Stream (Data Stream Operator)
- [`DSAggregate_Window()`](http://michael.hahsler.net/stream/reference/DSAggregate_Window.md)
  : Sliding Window (Data Stream Operator)
- [`DSClassifier()`](http://michael.hahsler.net/stream/reference/DSClassifier.md)
  : Abstract Class for Data Stream Classifiers
- [`DSClassifier_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSClassifier_SlidingWindow.md)
  : DSClassifier_SlidingWindow – Data Stream Classifier Using a Sliding
  Window
- [`DSRegressor()`](http://michael.hahsler.net/stream/reference/DSRegressor.md)
  : Abstract Class for Data Stream Regressors
- [`DSRegressor_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSRegressor_SlidingWindow.md)
  : DSRegressor_SlidingWindow – Data Stream Regressor Using a Sliding
  Window
- [`DSOutlier()`](http://michael.hahsler.net/stream/reference/DSOutlier.md)
  : Abstract Class for Data Stream Outlier Detectors

## Clustering

- [`DSC()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_centers()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_weights()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_copy()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`nclusters()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_microclusters()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_microweights()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_macroclusters()`](http://michael.hahsler.net/stream/reference/DSC.md)
  [`get_macroweights()`](http://michael.hahsler.net/stream/reference/DSC.md)
  : Data Stream Clustering Base Class

- [`DSC_Micro()`](http://michael.hahsler.net/stream/reference/DSC_Micro.md)
  : Abstract Class for Micro Clusterers (Online Component)

- [`DSC_Macro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md)
  [`microToMacro()`](http://michael.hahsler.net/stream/reference/DSC_Macro.md)
  : Abstract Class for Macro Clusterers (Offline Component)

- [`DSC_TwoStage()`](http://michael.hahsler.net/stream/reference/DSC_TwoStage.md)
  : TwoStage Clustering Process

- [`DSC_Static()`](http://michael.hahsler.net/stream/reference/DSC_Static.md)
  : Create as Static Copy of a Clustering

- [`DSC_Window()`](http://michael.hahsler.net/stream/reference/DSC_Window.md)
  : A sliding window from a Data Stream

- [`DSC_SlidingWindow()`](http://michael.hahsler.net/stream/reference/DSC_SlidingWindow.md)
  : DSC_SlidingWindow – Data Stream Clusterer Using a Sliding Window

- [`DSC_BICO()`](http://michael.hahsler.net/stream/reference/DSC_BICO.md)
  : BICO - Fast computation of k-means coresets in a data stream

- [`DSC_BIRCH()`](http://michael.hahsler.net/stream/reference/DSC_BIRCH.md)
  : Balanced Iterative Reducing Clustering using Hierarchies

- [`DSC_DBSTREAM()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md)
  [`get_shared_density()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md)
  [`change_alpha()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md)
  [`plot(`*`<DSC_DBSTREAM>`*`)`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md)
  [`DSOutlier_DBSTREAM()`](http://michael.hahsler.net/stream/reference/DSC_DBSTREAM.md)
  : DBSTREAM Clustering Algorithm

- [`DSC_DStream()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md)
  [`get_attraction()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md)
  [`plot(`*`<DSC_DStream>`*`)`](http://michael.hahsler.net/stream/reference/DSC_DStream.md)
  [`DSOutlier_DStream()`](http://michael.hahsler.net/stream/reference/DSC_DStream.md)
  : D-Stream Data Stream Clustering Algorithm

- [`DSC_evoStream()`](http://michael.hahsler.net/stream/reference/DSC_evoStream.md)
  : evoStream - Evolutionary Stream Clustering

- [`DSC_Sample()`](http://michael.hahsler.net/stream/reference/DSC_Sample.md)
  : Extract a Fixed-size Sample from a Data Stream

- [`DSC_DBSCAN()`](http://michael.hahsler.net/stream/reference/DSC_DBSCAN.md)
  : DBSCAN Macro-clusterer

- [`DSC_EA()`](http://michael.hahsler.net/stream/reference/DSC_EA.md) :
  Reclustering using an Evolutionary Algorithm

- [`DSC_Hierarchical()`](http://michael.hahsler.net/stream/reference/DSC_Hierarchical.md)
  : Hierarchical Micro-Cluster Reclusterer

- [`DSC_Kmeans()`](http://michael.hahsler.net/stream/reference/DSC_Kmeans.md)
  : Kmeans Macro-clusterer

- [`DSC_Reachability()`](http://michael.hahsler.net/stream/reference/DSC_Reachability.md)
  : Reachability Micro-Cluster Reclusterer

- [`DSC_R()`](http://michael.hahsler.net/stream/reference/DSC_R.md)
  [`update(`*`<DSC_R>`*`)`](http://michael.hahsler.net/stream/reference/DSC_R.md)
  : Abstract Class for Implementing R-based Clusterers

- [`get_assignment()`](http://michael.hahsler.net/stream/reference/get_assignment.md)
  :

  Assignment Data Points to Clusters
  [deprecated](https://rdrr.io/r/base/Deprecated.html)

- [`prune_clusters()`](http://michael.hahsler.net/stream/reference/prune_clusters.md)
  : Prune Clusters from a Clustering

- [`recluster()`](http://michael.hahsler.net/stream/reference/recluster.md)
  : Re-clustering micro-clusters

- [`predict(`*`<DST>`*`)`](http://michael.hahsler.net/stream/reference/predict.md)
  [`predict(`*`<DSC>`*`)`](http://michael.hahsler.net/stream/reference/predict.md)
  : Make a Prediction for a Data Stream Mining Task

- [`update(`*`<DST>`*`)`](http://michael.hahsler.net/stream/reference/update.md)
  : Update a Data Stream Mining Task Model with Points from a Stream

## Evaluation and visualization

- [`evaluate_static()`](http://michael.hahsler.net/stream/reference/evaluate.md)
  [`evaluate_stream()`](http://michael.hahsler.net/stream/reference/evaluate.md)
  : Evaluate a Data Stream Mining Task
- [`evaluate_static(`*`<DSC>`*`)`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md)
  [`evaluate_stream(`*`<DSC>`*`)`](http://michael.hahsler.net/stream/reference/evaluate.DSC.md)
  : Evaluate a Stream Clustering Task
- [`agreement()`](http://michael.hahsler.net/stream/reference/agreement.md)
  : Agreement-based Measures for Clustering
- [`plot(`*`<DSD>`*`)`](http://michael.hahsler.net/stream/reference/plot.DSD.md)
  : Plot Data Stream Data
- [`plot(`*`<DSC>`*`)`](http://michael.hahsler.net/stream/reference/plot.DSC.md)
  : Plot Results of a Data Stream Clustering
- [`animate_data()`](http://michael.hahsler.net/stream/reference/animate_data.md)
  : Animates the Plotting of a Data Streams
- [`animate_cluster()`](http://michael.hahsler.net/stream/reference/animate_cluster.md)
  : Animates Plots of the Clustering Process

## Stream I/O and utilities

- [`write_stream()`](http://michael.hahsler.net/stream/reference/write_stream.md)
  : Write a Data Stream to a File
- [`saveDSC()`](http://michael.hahsler.net/stream/reference/read_saveDSC.md)
  [`readDSC()`](http://michael.hahsler.net/stream/reference/read_saveDSC.md)
  : Save and Read DSC Objects
- [`` `%>%` ``](http://michael.hahsler.net/stream/reference/pipe.md) :
  Pipe operator
