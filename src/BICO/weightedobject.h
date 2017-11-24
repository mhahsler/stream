#ifndef WEIGHTEDOBJECT_H
#define WEIGHTEDOBJECT_H

namespace CluE
{

/**
 * @brief Abstract base class for weighted objects
 */
class WeightedObject
{
public:
	virtual double getWeight() const = 0;
	virtual void setWeight(double w) = 0;
};

}

#endif
