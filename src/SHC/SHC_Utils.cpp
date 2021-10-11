#include "SHC_Utils.hpp"

string generate_hex(const unsigned int len) {
  stringstream ss;
  for(unsigned i = 0; i < len; i++) {
    mt19937 eng((unsigned)chrono::high_resolution_clock::now().time_since_epoch().count());
    const unsigned int rc = eng() % 255;
    stringstream hexstream;
    hexstream << hex << rc;
    auto hex = hexstream.str();
    ss << (hex.length() < 2 ? '0' + hex : hex);
  }
  return ss.str();
}

string formatDouble(double value,int precision) {
    stringstream ss;
    ss << fixed << setprecision(precision) << value;
    return ss.str();
}
