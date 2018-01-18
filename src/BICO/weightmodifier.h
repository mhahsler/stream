#ifndef WEIGHTMODIFIER_H
#define WEIGHTMODIFIER_H

namespace CluE
{

/**
 * @brief Abstract base class to modify the weight of weighted objects.
 * 
 * @ingroup base_classes
 */
template<typename T> class WeightModifier
{
public:
	virtual ~WeightModifier()
	{
	}

	/**
	 * @brief make an exact copy of this object
	 * The clone method creates a copy of this object and returns a pointer to the new instance.
	*/
	virtual WeightModifier<T>* clone() const = 0;

	virtual double getWeight(T&) = 0;
	virtual void setWeight(T&, double) = 0;
};

}

#endif