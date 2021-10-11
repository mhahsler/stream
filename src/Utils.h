#ifndef SHC_H_Utils
#define SHC_H_Utils
 
#include <iostream>
#include <math.h>
#include "RcppEigen.h"
using namespace Eigen;
using namespace Rcpp;

VectorXd calculateNewMean(VectorXd mean,VectorXd newElement,int elements);
MatrixXd calculateCovariance(VectorXd oldMean,VectorXd newMean,MatrixXd oldCovariance,int elements,VectorXd newElement,bool isInversion);
MatrixXd constructVirtualCovariance(VectorXd virtualVariance);
MatrixXd chooseUniOrMulti(MatrixXd covariance,VectorXd vvar,int elms);
double mahalanobisDistance(VectorXd mean,VectorXd newElement,MatrixXd covariance,VectorXd virtualVariance,int elements,bool isInversion);
double calculateEuclidean(VectorXd x,VectorXd y);
double calculateMDSeparation(VectorXd mean1,MatrixXd cov1,VectorXd vvar1,int elms1,bool isInv1,VectorXd mean2,MatrixXd cov2,VectorXd vvar2,int elms2,bool isInv2,double th1,double th2);
bool isCovarianceInvertible(MatrixXd covariance);
MatrixXd invertCovariance(MatrixXd covariance);

double shc_MDSeparation(NumericVector mean1, NumericMatrix covariance1, NumericVector virtualVariance1, int N1, bool isInversion1, double th1,
                        NumericVector mean2, NumericMatrix covariance2, NumericVector virtualVariance2, int N2, bool isInversion2, double th2);
NumericVector shc_CalculateNewMean(NumericVector mean, NumericVector newElement, int N);
NumericMatrix shc_CalculateCovariance(NumericVector oldMean, NumericVector newMean, NumericMatrix oldCovariance, int N, 
                                      NumericVector newElement, bool isInversion);

#endif /* SHC_H_Utils */
