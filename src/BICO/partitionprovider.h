#ifndef PARTITIONPROVIDER_H
#define PARTITIONPROVIDER_H

#include "solutionprovider.h"

#include <vector>

namespace CluE
{

/**
 * @brief Abstract base class to access results of partition based clustering algorithms.
 *
 * @ingroup base_classes
 */
template<typename T> class PartitionProvider
{
public:

	virtual ~PartitionProvider()
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
	* @brief Returns the cardinality of the specified cluster from the computed clustering.
	*/
	virtual unsigned int clustersize(unsigned int solutionIndex, unsigned int partitionIndex) const = 0;

	/**
	* @brief Returns a pointer to a particular element from the specified cluster and clustering.
	*/
	virtual T* element(unsigned int solutionIndex, unsigned int partitionIndex, unsigned int elementIndex) const = 0;

	/**
	* @brief Returns a vector of pointers to the elements of a particular cluster from the specified
	*        clustering.
	*/
	virtual std::vector<T*> cluster(unsigned int solutionIndex, unsigned int partitionIndex) const = 0;

	/**
	* @brief Returns the specified clustering as a vector of vector of pointers to the elements.
	*/
	virtual std::vector<std::vector<T*> > clustering(unsigned int solutionIndex) const = 0;

	/**
	* @brief Does a dynamic cast of the given SolutionProvider to a PartitionProvider.
	* @return NULL if the SolutionProvider is not a PartitionProvider instance
	*/
	static PartitionProvider<T>* toPartitionProvider(SolutionProvider* s)
	{
		return dynamic_cast<PartitionProvider<T>*>(s);
	}
};

}

#endif
