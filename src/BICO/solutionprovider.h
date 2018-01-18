#ifndef SOLUTIONPROVIDER_H
#define SOLUTIONPROVIDER_H

#include <string>

namespace CluE
{

/**
 * @brief Abstract base class for algorithm solutions.
 *
 * Abstract base class for all algorithm's solutions (clustering, coreset, seeding, ...).
 * 
 * @ingroup base_classes
 */
class SolutionProvider
{
public:
	virtual ~SolutionProvider()
	{
	}

	/**
	* @brief returns the time needed for the last computation
	*
	* @return time in seconds needed for last call to compute()
	*/
	virtual double computationtime() const = 0;
};

}

#endif
