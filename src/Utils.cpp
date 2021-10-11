#include "Utils.h"
using namespace Eigen;
using namespace Rcpp;

VectorXd calculateNewMean(VectorXd mean,VectorXd newElement,int elements) {
	VectorXd t1 = (newElement - mean) / (elements + 1);
	VectorXd t2 = mean + t1;
	return t2;
}

MatrixXd calculateCovariance(VectorXd oldMean,VectorXd newMean,MatrixXd oldCovariance,int elements,VectorXd newElement,bool isInversion) {
  if(!isInversion) {
    MatrixXd B = (((newElement - oldMean) * (newElement - newMean).transpose()) - oldCovariance) / (elements + 1);
    return oldCovariance + B;
  } else {
    MatrixXd u = (newElement-oldMean)/sqrt(elements);
    MatrixXd vt = ((newElement-newMean)/sqrt(elements)).transpose();
    MatrixXd v1 = oldCovariance*u*vt*oldCovariance;
    MatrixXd v2 = vt*oldCovariance*u;
    MatrixXd res = oldCovariance-v1/((double)1+v2(0,0));
    double X = (double)elements/((double)elements+(double)1);
    MatrixXd res2 = res/X;
    return res2;
  }
}

MatrixXd constructVirtualCovariance(VectorXd virtualVariance) {
	MatrixXd vcov = MatrixXd::Zero(virtualVariance.size(), virtualVariance.size());
	for(int i=0;i<virtualVariance.size();i++)
		vcov(i,i) = virtualVariance(i);
	return vcov;
}

MatrixXd chooseUniOrMulti(MatrixXd covariance,VectorXd vvar,int elms) {
	if(elms < 2) return constructVirtualCovariance(vvar);

	FullPivLU<MatrixXd> lu(covariance);
	if(lu.isInvertible()) return covariance;

	MatrixXd vcov = constructVirtualCovariance(vvar);
	for(int i=0;i<covariance.cols();i++)
		if(covariance(i,i)!=(float)0.0) vcov(i,i) = covariance(i,i);
	return vcov;
}

bool isCovarianceInvertible(MatrixXd covariance) {
  FullPivLU<MatrixXd> lu(covariance);
  return(lu.isInvertible());
}

MatrixXd invertCovariance(MatrixXd covariance) {
  if(isCovarianceInvertible(covariance))
    return(covariance.inverse());
  else
    return(MatrixXd::Zero(covariance.rows(),covariance.cols()));
}

double mahalanobisDistance(VectorXd mean,VectorXd newElement,MatrixXd covariance,VectorXd virtualVariance,
		int elements,bool isInversion) {
  MatrixXd icovariance;
  if(!isInversion)
	  icovariance = chooseUniOrMulti(covariance, virtualVariance, elements).inverse();
  else
    icovariance = covariance;
	VectorXd delta = newElement - mean;
	double N0 = delta.transpose() * (icovariance * delta);
	return sqrtf(N0);
}

double calculateEuclidean(VectorXd x,VectorXd y) {
	return (x - y).norm();
}

double calculateMDSeparation(VectorXd mean1,MatrixXd cov1,VectorXd vvar1,int elms1,bool isInv1,
		VectorXd mean2,MatrixXd cov2,VectorXd vvar2,int elms2,bool isInv2,double th1,double th2) {
	double md1 = mahalanobisDistance(mean1, mean2, cov1, vvar1, elms1, isInv1);
	double p1 = th1 / md1;
  double md2 = mahalanobisDistance(mean2, mean1, cov2, vvar2, elms2, isInv2);
  double p2 = th2 / md2;
  if((p1+p2)==0) return (double)0.0;
  return 1/(p1+p2);
}

// [[Rcpp::export]]
double shc_MDSeparation(NumericVector mean1, NumericMatrix covariance1, NumericVector virtualVariance1, int N1, bool isInversion1, double th1,
                        NumericVector mean2, NumericMatrix covariance2, NumericVector virtualVariance2, int N2, bool isInversion2, double th2) {
  double mdsep = calculateMDSeparation(as<VectorXd>(mean1),as<MatrixXd>(covariance1),as<VectorXd>(virtualVariance1),N1,isInversion1,
                                       as<VectorXd>(mean2),as<MatrixXd>(covariance2),as<VectorXd>(virtualVariance2),N2,isInversion2, 
                                       th1,th2);
  return mdsep;
}

// [[Rcpp::export]]
double shc_MutualMinMahalanobis(NumericVector mean1, NumericMatrix covariance1, NumericVector virtualVariance1, int N1, bool isInversion1,
                                NumericVector mean2, NumericMatrix covariance2, NumericVector virtualVariance2, int N2, bool isInversion2) {
  double md1 = mahalanobisDistance(as<VectorXd>(mean2),as<VectorXd>(mean1),as<MatrixXd>(covariance2),as<VectorXd>(virtualVariance2),
                                   N2,isInversion2);
  double md2 = mahalanobisDistance(as<VectorXd>(mean1),as<VectorXd>(mean2),as<MatrixXd>(covariance1),as<VectorXd>(virtualVariance1),
                                   N1,isInversion1);
  return md1<=md2 ? md1 : md2;
}

// [[Rcpp::export]]
NumericVector shc_CalculateNewMean(NumericVector mean, NumericVector newElement, int N) {
  VectorXd res = calculateNewMean(as<VectorXd>(mean),as<VectorXd>(newElement),N);
  NumericVector res_nm(wrap(res));
  return res_nm;
}

// [[Rcpp::export]]
NumericMatrix shc_CalculateCovariance(NumericVector oldMean, NumericVector newMean,
                                      NumericMatrix oldCovariance, int N, NumericVector newElement,
                                      bool isInversion) {
  MatrixXd mat_newCovariance = calculateCovariance(as<VectorXd>(oldMean),as<VectorXd>(newMean),as<MatrixXd>(oldCovariance),
                                                   N,as<VectorXd>(newElement),isInversion);
  NumericMatrix res(wrap(mat_newCovariance));
  return res;
}