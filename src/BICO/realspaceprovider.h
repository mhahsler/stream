#ifndef REALSPACEPROVIDER_H
#define REALSPACEPROVIDER_H

#include "point.h"
#include "euclideanspaceprovider.h"

namespace CluE
{

/**
 * @brief Provides euclidean vector space features for Point.
 *
 * @ingroup pointrelated_classes
 */
class RealSpaceProvider : public EuclideanSpaceProvider<Point>
{
public:
	RealSpaceProvider(unsigned int dimension);

	virtual RealSpaceProvider* clone() const;

	virtual V nullVector() const;

private:
	unsigned int dimension;
};

}

#endif
