#ifndef CLUEALGORITHM_H
#define CLUEALGORITHM_H

#include "solutionprovider.h"

/**
* @brief namespace for the CluE library
*/
namespace CluE
{

/**
 * @brief Abstract base class for algorithms.
 *
 * @ingroup base_classes
 */
class Algorithm
{
public:
	virtual ~Algorithm()
	{
	}

	/**
	 * @brief Runs the algorithm and returns the computed solution.
	 *
	 * Implementing classes override this method with the computation of a SolutionProvider
	 * instance whose reference is returned. The responibility for destructing the instance lies
	 * with the caller.
	 */
	virtual SolutionProvider* compute() = 0;
};

}

#endif
