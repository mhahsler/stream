#ifndef EVALUATION_H
#define EVALUATION_H

namespace CluE
{

/**
 * @brief Abstract base class for clustering evaluations.
 * 
 * Clustering evaluations should derive from this class.
 * 
 * @ingroup base_classes
 */
class Evaluation
{
public:
	virtual ~Evaluation()
	{
	}
};

}

#endif