#ifndef MEASURESETTER_H
#define MEASURESETTER_H

#include "dissimilaritymeasure.h"

namespace CluE
{
/**
 * @brief Interface to propagate the ability to set a DissimilarityMeasure
 *
 * @ingroup base_classes
 */
template<typename T> class MeasureSetter
{
public:
	virtual void setMeasure(DissimilarityMeasure<T> const *measure) = 0;
};
}

#endif
