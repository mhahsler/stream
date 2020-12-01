\docType{class}
\name{DSC_SinglePass-class}
\alias{DSC_SinglePass}
\title{Abstract Class for Single-Pass Clusterers}
\section{Methods}{
\describe{
\item{\code{get_assignments(dsc, points, type=c("auto", "micro", "macro"), method=c("auto", "nn", "model"), ...)}}{
A method that needs to be re-implemented by all single-pass clusterers. In this method, each data instance retrieved from an input data stream must be processed in two steps:
\enumerate{
\item{\bold{Classification}, which contributes to the returning data frame,}
\item{\bold{Model update}}
}
}
}}
\description{
The abstract class for all single-pass clusterers. Cannot be instantiated. Must be inherited.
Outlier detecting clusterers are usually single-pass clusterers, since the outlier detection event must be reported as soon as possible back to the caller.
}
\author{
    Dalibor Krleža
}
