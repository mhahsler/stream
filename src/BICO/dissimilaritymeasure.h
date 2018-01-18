#ifndef DISSIMILARITYMEASURE_H
#define DISSIMILARITYMEASURE_H

namespace CluE
{

/**
 * @brief Abstract base class for dissimilarity measurement.
 * 
 * @ingroup base_classes
 */
template<typename T> class DissimilarityMeasure
{
public:

	virtual ~DissimilarityMeasure()
	{
	}
	
	virtual DissimilarityMeasure<T>* clone() const = 0;

	/**
	 * @brief Computes the dissimilarity between the two given objects.
	 */
	virtual double dissimilarity(T const&, T const&) const = 0;
};

}

#endif
