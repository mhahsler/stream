#ifndef CLUSTERDISSIMILARITYMEASURE_H
#define CLUSTERDISSIMILARITYMEASURE_H

#include <vector>

namespace CluE
{

/**
 * @brief Abstract base class for cluster dissimilarity measurement.
 * @ingroup base_classes
 */
template<typename T> class ClusterDissimilarityMeasure
{
public:
	virtual ~ClusterDissimilarityMeasure()
	{
	}

	virtual ClusterDissimilarityMeasure<T>* clone() const = 0;

	/**
	 * @brief Computes the dissimilarity between the two given clusters.
	 */
	virtual double dissimilarity(std::vector<T*> const&, std::vector<T*> const&) = 0;
};

}

#endif