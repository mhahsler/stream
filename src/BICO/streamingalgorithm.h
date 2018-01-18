#ifndef CLUESTREAMINGALGORITHM_H
#define CLUESTREAMINGALGORITHM_H

#include "algorithm.h"

namespace CluE
{
/**
 * @brief Abstract base class for streaming algorithms
 *
 * 1. Pass the stream elemets through the << operator.
 * 2. Use compute() to request the result.
 *
 * @ingroup base_classes
 */
template<typename T> class StreamingAlgorithm : public Algorithm
{
public:
	/**
	 * @brief Streaming operator
	 */
	virtual StreamingAlgorithm<T>& operator<<(T const & element) = 0;
};

}

#endif
