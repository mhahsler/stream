#ifndef PARTITIONEVALUATION_H
#define PARTITIONEVALUATION_H

#include "evaluation.h"
#include "partitionprovider.h"

#include <vector>

namespace CluE
{

/**
 * @brief Abstract class for partition-based evaluation algorithms.
 *
 * Evaluation algorithms requiring partitions for calculating the input's cost should derive from this class.
 *
 * @ingroup base_classes
 */
template<typename T> class PartitionEvaluation : virtual public Evaluation
{
public:
	virtual ~PartitionEvaluation()
	{
	}

	/**
	 * @brief Calculates the cost of a given partitioning.
	 */
	virtual double partitioncost(std::vector<std::vector<T*> > const &partitioning) const = 0;
	/**
	 * @overload
	 */
	virtual double partitioncost(PartitionProvider<T> const &partitioning, unsigned int solutionIndex) const = 0;

	/**
	 * @brief Calculates the cost of a given partition.
	 */
	virtual double partitioncost(std::vector<T*> const &partition) const = 0;
	/**
	 * @overload
	 */
	virtual double partitioncost(PartitionProvider<T> const &partitioning, unsigned int solutionIndex, unsigned int partitionIndex) const = 0;
};

}

#endif
