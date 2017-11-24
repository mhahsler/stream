#ifndef ATTRIBUTECALCULATOR_H
#define ATTRIBUTECALCULATOR_H

namespace CluE
{

/**
 * @brief Abstract base class for attribute calculation (e.g. diameter).
 * 
 * @ingroup base_classes
 */
template<typename T> class AttributeCalculator
{
public:	
	virtual ~AttributeCalculator()
	{
	}
	
	virtual AttributeCalculator<T>* clone() const = 0;
	
	/**
	 * @brief Computes a characteristic attribute of a given object.
	 */
	virtual double calculate(T const&) const = 0;
};

}

#endif
