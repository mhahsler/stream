#ifndef POINTCENTROID_H
#define POINTCENTROID_H

#include "proxygenerator.h"
#include "point.h"

#include <vector>

namespace CluE
{

/**
 * @brief Center of gravity for Point.
 *
 * ProxyGenerator<Point> class for computing a weighted center of gravity for a vector of @ref Point
 * objects.
 *
 * @ingroup pointrelated_classes
 */
class PointCentroid : public ProxyGenerator<Point>
{
public:

	virtual PointCentroid* clone() const;

	/**
	* This method generates the center of gravity for the given Point objects
	* using the constructor @ref Point::Point.
	*/
	virtual Point generate(std::vector<Point*> const&) const;
};

}

#endif
