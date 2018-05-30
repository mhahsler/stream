//
// Created by Dede on 11.09.2016.
//

#include "Util.hpp"
namespace Utility{
double Util::innerProduct(Rcpp::NumericVector x,
                                 Rcpp::NumericVector y) {
  double erg=0;
  for(int i = 0;i<x.size();i++){
    erg = erg+ (x[i]*y[i]);
  }
  return erg;
}

double Util::vectorLengthEuclid(Rcpp::NumericVector x) {
  double val = innerProduct(x,x);
  return sqrt(val);
}

double Util::vectorLengthManhatten(Rcpp::NumericVector x) {
  Rcpp::NumericVector result;
  result = abs(x);
  double erg = std::accumulate(result.begin(),result.end(), 0.0);
  return erg;
}
}
