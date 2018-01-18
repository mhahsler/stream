#ifndef SQUAREDL2METRIC_H
#define SQUAREDL2METRIC_H

#include "dissimilaritymeasure.h"
#include "point.h"

namespace CluE
{

/**
 * @brief Squared L2 metric for Point.
 *
 * @ingroup pointrelated_classes
 */
class SquaredL2Metric : public DissimilarityMeasure<Point>
{
public:
	virtual SquaredL2Metric* clone() const;

	/**
	* Computes the squared l2-distance between the two given Point instances.
	*/
	virtual double dissimilarity(Point const&, Point const&) const;
};

}

#endif
