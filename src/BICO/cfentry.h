#ifndef CFENTRY_H
#define CFENTRY_H

#include <type_traits>

#include "weightmodifier.h"

namespace CluE
{

/**
 * @brief Clustering feature tree entry
 *
 * @note This class expects the following overloaded operators:
 * - + : T x T -> T and +=: (vector) sum
 * - - : T x T -> T and -=: (vector) subtraction
 * - * : T x T -> double: dot product
 *
 * Clustering tree feature as described in
 * T. Zhang, R. Ramakrishan and M. Livny. "BIRCH: A New Data Clustering Algorithm and Its Applications". <em>Data Mining and Knowledge Discovery</em>, 10.1023/A:1009783824328, 1997.
 */
template<typename T> struct CFEntry
{
	/**
	 * @brief Number of points contained in the feature
	 */
	size_t number;

	/**
	 * @brief Linear sum
	 */
	T LS;

	/**
	 * @brief Squared sum
	 */
	double SS;

	const bool isWeighted;

	CFEntry(size_t number, T ls, double ss);

	CFEntry& operator+=(CFEntry const & x);
	CFEntry& operator-=(CFEntry const & x);
	CFEntry operator+(CFEntry const & x) const;
	CFEntry operator-(CFEntry const & x) const;

	/**
	 * @brief Inserts a point
	 */
	void insert(T const & x);

	/**
	 * @brief Removes a point
	 */
	 void remove(T const & x);

	/**
	 * @brief Returns the center of gravity
	 */
	T cog();

	/**
	 * @brief 1-means clustering cost
	 */
	double kMeansCost(T const & center);
};

template<typename T> CFEntry<T>::CFEntry(size_t number, T ls, double ss) :
	number(number),
	LS(ls),
	SS(ss),
	isWeighted(std::is_base_of<WeightedObject, T>::value)
{
}

template<typename T> CFEntry<T>& CFEntry<T>::operator+=(CFEntry<T> const & x)
{
	number += x.number;
	LS += x.LS;
	SS += x.SS;
	return *this;
}

template<typename T> CFEntry<T>& CFEntry<T>::operator -=(CFEntry<T> const & x)
{
	number -= x.number;
	LS -= x.LS;
	SS -= x.SS;
	return *this;
}

template<typename T> CFEntry<T> CFEntry<T>::operator+(CFEntry<T> const & x) const
{
	return CFEntry(*this) += x;
}

template<typename T> CFEntry<T> CFEntry<T>::operator-(CFEntry<T> const & x) const
{
	return CFEntry(*this) -= x;
}

template<typename T> void CFEntry<T>::insert(T const & x)
{
	double weight = 1.0;
	if(isWeighted)
	{
		WeightedObject const * wm = static_cast<WeightedObject const *>(&x);
		weight = wm->getWeight();
	}
	number += weight;
	LS += weight * x;
	SS += weight * (x*x);
}

template<typename T> void CFEntry<T>::remove(T const & x)
{
	double weight = 1.0;
	if(isWeighted)
	{
		WeightedObject const * wm = static_cast<WeightedObject const *>(&x);
		weight = wm->getWeight();
	}
	number -= weight;
	LS -= weight * x;
	SS -= weight * (x*x);
}

template<typename T> T CFEntry<T>::cog()
{
	return (1.0 / number) * LS;
}

template<typename T> double CFEntry<T>::kMeansCost(T const & center)
{
	return SS - 2 * (LS * center) + number * (center * center);
}

}

#endif
