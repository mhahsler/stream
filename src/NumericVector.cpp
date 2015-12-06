#include <Rcpp.h>
#include "NumericVector.h"

namespace Rcpp {
std::ostream& operator<<(std::ostream& os, Rcpp::NumericVector const& obj) {
  os << "[ ";
  for (int i=0; i < obj.length(); i++) os << obj[i] << " ";
  os << "]";
  return os;
}
}
