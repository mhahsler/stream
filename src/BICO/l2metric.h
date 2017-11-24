#ifndef L2METRIC_H
#define L2METRIC_H

#include "dissimilaritymeasure.h"
#include "point.h"

namespace CluE
{

/**
 * @brief L2 metric for Point
 *
 * @ingroup pointrelated_classes
 */
class L2Metric : public DissimilarityMeasure<Point>
{
public:
	virtual L2Metric* clone() const;

	/**
	 * @brief Computes the l2-distance between the two given Point instances.
     */
	virtual double dissimilarity(Point const&, Point const&) const;
};

}

#endif
