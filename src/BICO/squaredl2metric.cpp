#include "squaredl2metric.h"
#include "point.h"

using namespace CluE;

SquaredL2Metric* SquaredL2Metric::clone() const
{
	return new SquaredL2Metric(*this);
}

double SquaredL2Metric::dissimilarity(Point const& p1, Point const& p2) const
{
    return p1.squaredL2distance(p2);
}
