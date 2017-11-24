#ifndef COMBINEDEVALUATION_H
#define COMBINEDEVALUATION_H

#include "partitionprovider.h"
#include "proxyprovider.h"
#include "discreteproxyprovider.h"

#include <vector>

namespace CluE {

/**
 * @brief Abstract class. Proxy based evaluation algorithms may be optimized by deriving from this class.
 *
 * Evaluation algorithms requiring proxies and partitions for calculating the input's cost may derive from this class.
 * Using an already existing partitioning instead of calucalting it based on a given set of proxies will speed up computation time.
 *
 * @ingroup base_classes
 */
template<typename T> class CombinedEvaluation : virtual public Evaluation {
public:
	virtual ~CombinedEvaluation() {
	}

	/**
	 * @brief Calculates the cost related to the proxies, based on the partitions.
	 * @note There are separate overloaded versions of this method for use with discrete proxies.
	 */
	virtual double combinedcost(std::vector<std::vector<T*> > const& partitioning, std::vector<T> const& proxies) const = 0;
	/**
	 * @overload
	 * @param solutionIndex PartitionProvider and ProxyProvider index.
	 */
	virtual double combinedcost(PartitionProvider<T> const &partitioning, ProxyProvider<T> const &proxies, unsigned int solutionIndex) const = 0;
	/**
	 * @overload
	 */
	virtual double combinedcost(std::vector<T*> const &partition, T const &proxy) const = 0;
	/**
	 * @overload
	 * @param solutionIndex PartitionProvider and ProxyProvider solution index.
	 * @param elementIndex Partition and proxy index.
	 */
	virtual double combinedcost(PartitionProvider<T> const &partitioning, ProxyProvider<T> const &proxies, unsigned int solutionIndex, unsigned int elementIndex) const = 0;

	/**
	 * @brief Calculates the cost related to the discrete proxies, based on the partitions.
	 */
	virtual double combinedcost(std::vector<std::vector<T*> > const& partitioning, std::vector<T*> const& proxies) const = 0;
	/**
	 * @overload
	 * @param solutionIndex PartitionProvider and ProxyProvider solution index.
	 */
	virtual double combinedcost(PartitionProvider<T> const &partitioning, DiscreteProxyProvider<T> const &proxies, unsigned int solutionIndex) const = 0;
	/**
	 * @overload
	 */
	virtual double combinedcost(std::vector<T*> const &partition, T const * const proxy) const;
	/**
	 * @overload
	 * @param solutionIndex PartitionProvider and ProxyProvider index.
	 * @param elementIndex Partition and proxy index.
	 */
	virtual double combinedcost(PartitionProvider<T> const &partitioning, DiscreteProxyProvider<T> const &proxies, unsigned int solutionIndex, unsigned int elementIndex) const = 0;
};

template<typename T> inline double CombinedEvaluation<T>::combinedcost(std::vector<T*> const& cluster, T const * const proxy) const {
	return combinedcost(cluster, *proxy);
}

}

#endif
