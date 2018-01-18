#ifndef CFRENTRY_H
#define CFRENTRY_H

#include "cfentry.h"

namespace CluE
{

/**
 * @brief Clustering feature with representation point
 * 
 * @note This class expects the following overloaded operators:
 * - + : T x T -> T and +=: (vector) sum
 * - - : T x T -> T and -=: (vector) subtraction
 * - * : T x T -> double: dot product
 */
template<typename T> struct CFREntry : CFEntry<T>
{
	CFREntry(size_t number, T ls, double ss, T representative);
	void insertWeighted(T const & x);
	
	T representative;
};

template<typename T> CFREntry<T>::CFREntry(size_t number, T ls, double ss, T representative) :
	CFEntry<T>(number, ls, ss),
	representative(representative)
{
}

}

#endif