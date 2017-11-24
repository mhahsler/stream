#ifndef DISCRETEPROXYPROVIDER_H
#define DISCRETEPROXYPROVIDER_H

#include "solutionprovider.h"

#include <vector>

namespace CluE
{

/**
 * @brief Abstract base class to access the results of proxy / center based clustering algorithms.
 *
 * @ingroup base_classes
 */
template<typename T> class DiscreteProxyProvider
{
public:

	virtual ~DiscreteProxyProvider()
	{
	}

	/**
	* @brief returns the number of available solutions
	*
	* The algorthm may compute more than one solution of possibly different size, where size means
	* number of computed clusters, proxies (e.g. cluster centers) or the size of a coreset.
	* The sizes can be retrieved by a call to size_of_solution().
	*/
	virtual unsigned int number_of_solutions() const = 0;

	/**
	* @brief returns the size of a particular solution
	*
	* @param index number between 0 and @ref number_of_solutions()-1
	* @return the size for the requested clustering
	*/
	virtual unsigned int size_of_solution(unsigned int index) const = 0;

	/**
	* @brief Returns a pointer to the proxy for the specified clustering and cluster.
	*
	* Returns a pointer to the element of the input set that was computed to be the proxy for
	* cluster number proxyIndex in clustering number solutionIndex.
	*/
	virtual T* discrete_proxy(unsigned int solutionIndex, unsigned int proxyIndex) const = 0;

	/**
	* @brief Returns a vector of pointers to the proxies for the specified clustering.
	*
	* Returns a vector of pointers to the elements of the input set that were computed to be the
	* proxies for clustering number proxyIndex.
	*/
	virtual std::vector<T*> discrete_proxies(unsigned int solutionIndex) const = 0;

	/**
	* @brief Does a dynamic cast of the given SolutionProvider to a DiscreteProxyProvider.
	* @return NULL if the SolutionProvider is not a DiscreteProxyProvider instance
	*/
	static DiscreteProxyProvider<T>* toDiscreteProxyProvider(SolutionProvider* s)
	{
		return dynamic_cast<DiscreteProxyProvider<T>*>(s);
	}
};

}

#endif
