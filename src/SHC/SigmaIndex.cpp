#include "SigmaIndex.hpp"

SigmaIndex_Exception::SigmaIndex_Exception(const char *msg) {
    this->msg=msg;
}
const char *SigmaIndex_Exception::what() const throw() {
    return this->msg;
}

