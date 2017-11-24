#ifndef POINTWEIGHTMODIFIER_H
#define POINTWEIGHTMODIFIER_H

#include "weightmodifier.h"
#include "point.h"

namespace CluE
{

/**
 * @brief Modifies the weight of a Point.
 *
 * @ingroup pointrelated_classes
 */
class PointWeightModifier : public WeightModifier<Point>
{
public:
	virtual PointWeightModifier* clone() const;

	virtual double getWeight(Point&);
	virtual void setWeight(Point&, double);
};

inline
PointWeightModifier* PointWeightModifier::clone() const
{
	return new PointWeightModifier(*this);
}

inline
double PointWeightModifier::getWeight(Point& p)
{
    return p.getWeight();
}

inline
void PointWeightModifier::setWeight(Point& p, double w)
{
    p.setWeight(w);
}

}

#endif
