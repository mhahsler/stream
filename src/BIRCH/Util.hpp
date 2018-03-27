//
// Created by Dede on 11.09.2016.
//

#ifndef CFTREE_UTIL_HPP
#define CFTREE_UTIL_HPP


#include <numeric>
#include <algorithm>
#include <vector>
#include <Rcpp.h>
#include <math.h>

namespace Utility{
class Util {
public:
  static double innerProduct(Rcpp::NumericVector x,
                             Rcpp::NumericVector y);
  static double vectorLengthEuclid(Rcpp::NumericVector x);
  static double vectorLengthManhatten(Rcpp::NumericVector x);
private:
  Util() {}
};

}

#endif //CFTREE_UTIL_HPP
