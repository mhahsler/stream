#include <Rcpp.h>

namespace Rcpp {
std::ostream& operator<<(std::ostream& os, Rcpp::NumericVector const& obj);
}
