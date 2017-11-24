#ifndef CLUSTERMEASURESETTER_H
#define CLUSTERMEASURESETTER_H

namespace CluE
{
/**
 * @brief Interface to propagate the ability to set a ClusterDissimilarityMeasure
 * 
 * @ingroup base_classes
 */
template<typename T> class ClusterMeasureSetter
{
	virtual void setMeasure(ClusterDissimilarityMeasure<T> const *measure) = 0;
};
}

#endif