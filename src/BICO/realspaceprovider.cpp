#include "realspaceprovider.h"

#include "invalidargumentexception.h"

namespace CluE
{

RealSpaceProvider::RealSpaceProvider(unsigned int dimension) :
	dimension(dimension)
{
}

RealSpaceProvider* RealSpaceProvider::clone() const
{
	return new RealSpaceProvider(*this);
}

Point RealSpaceProvider::nullVector() const
{
	std::vector<double> coords(dimension, 0);
	return Point(coords, 1);
}

}

