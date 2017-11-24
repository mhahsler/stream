#ifndef EUCLIDEANSPACEPROVIDER_H
#define EUCLIDEANSPACEPROVIDER_H

namespace CluE
{

/**
 * @brief Interface to extend a template type to provide euclidean vector space features.
 * 
 * @ingroup base_classes
 */
template<typename VectorType> class EuclideanSpaceProvider
{
public:
	typedef VectorType V;
	
	virtual EuclideanSpaceProvider<V>* clone() const = 0;
	
	virtual V nullVector() const = 0;
};

}

#endif