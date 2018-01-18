#ifndef PROXYPROVIDER_H
#define PROXYPROVIDER_H

#include <string>
#include "solutionprovider.h"

namespace CluE {

/**
 * @brief Abstract base class to access results of proxy / center based clustering algorithms.
 *
 * @ingroup base_classes
 */
template<typename T> class ProxyProvider {
public:

	virtual ~ProxyProvider() {
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
	* @brief returns the proxy for the specified clustering and cluster
	*
	* Returns the computed proxy for cluster number proxyIndex in clustering number solutionIndex.
	*/
	virtual T proxy(unsigned int solutionIndex, unsigned int proxyIndex) const = 0;

	/**
	* @brief returns the proxies for the specified clustering
	*
	* Returns the computed proxies for clustering number solutionIndex.
	*/
	virtual std::vector<T> proxies(unsigned int solutionIndex) const = 0;

	/**
	* @brief does a dynamic cast of the given SolutionProvider to a ProxyProvider
	* @return NULL if the SolutionProvider is not a ProxyProvider instance
	*/
	static ProxyProvider<T>* toProxyProvider(SolutionProvider* s) {
		return dynamic_cast<ProxyProvider<T>*>(s);
	}
};

}

#endif
