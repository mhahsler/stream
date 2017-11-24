#ifndef PROXYGENERATOR_H
#define PROXYGENERATOR_H

#include <vector>

namespace CluE
{

/**
 * @brief Abstract base class for mechanisms that compute a proxy or representative object for a given set of objects, e.g. a cluster center.
 * 
 * @ingroup base_classes
 */
template<typename T> class ProxyGenerator
{
public:

	virtual ~ProxyGenerator()
	{
	}
	
	virtual ProxyGenerator<T>* clone() const = 0;

	/**
	 * Generates a proxy for the given vector of objects.
	*/
	virtual T generate(std::vector<T*> const&) const = 0;
};

}

#endif
