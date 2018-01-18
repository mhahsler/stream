#ifndef DISCRETEBOUNDEDSPACE_H
#define DISCRETEBOUNDEDSPACE_H

namespace CluE
{

/**
 * @brief Interface to extend a template type to provide discrete (bounded) space {0, ..., n-1}^d features.
 * 
 * @ingroup base_classes
 */
template<typename VectorType, typename size_space> class DiscreteBoundedSpace
{
public:
	typedef VectorType V;
	
	virtual DiscreteBoundedSpace<V, size_space>* clone() const = 0;
	
	/**
	 * @brief Returns the vector represented by the given coordinates.
	 */
	virtual V getVector(std::vector<size_space> coordinates) const = 0;
	
	/**
	 * @brief Returns the coordinates of the given vector.
	 */
	virtual std::vector<size_space> getCoordinates(V const & vector) const = 0;
	
	/**
	 * @brief Space dimension
	 */
	virtual size_t dimension() const = 0;
	
	/**
	 * @brief Number of discrete coordinates per dimension
	 */
	virtual size_space n() const = 0;
	
	/**
	 * @brief Upper bound = n-1
	 */
	virtual size_space uBound() const
	{
		return n()-1;
	}
	
	/**
	 * @brief Returns the space's origin
	 */
	virtual VectorType origin() const = 0;
};

}

#endif
