#ifndef DISCRETEPROXYSOLUTION_H
#define DISCRETEPROXYSOLUTION_H

#include "solutionprovider.h"
#include "discreteproxyprovider.h"

#include <vector>

namespace CluE
{

/**
 * @brief Data structure for discrete proxies.
 *
 * This struct is for use in algorithms computing proxies.
 *
 * @ingroup data_structures
 */
template<typename T> struct DiscreteProxySolution : public SolutionProvider,
	public DiscreteProxyProvider<T>
{
public:

	DiscreteProxySolution();

	virtual ~DiscreteProxySolution()
	{
	}

	virtual double computationtime() const;
	virtual unsigned int number_of_solutions() const;
	virtual unsigned int size_of_solution(unsigned int) const;

	virtual T* discrete_proxy(unsigned int n, unsigned int c) const;
	virtual std::vector<T*> discrete_proxies(unsigned int n) const;

	double seconds;
	std::vector<std::vector<T*> > proxysets;
};

template<typename T> DiscreteProxySolution<T>::DiscreteProxySolution() : seconds()
{
}

template<typename T> double DiscreteProxySolution<T>::computationtime() const
{
	return seconds;
}

template<typename T> unsigned int DiscreteProxySolution<T>::number_of_solutions() const
{
	return this->proxysets.size();
}

template<typename T> unsigned int DiscreteProxySolution<T>::size_of_solution(unsigned int i) const
{
	if (i<this->proxysets.size())
		return this->proxysets[i].size();
	return 0;
}

template<typename T> T* DiscreteProxySolution<T>::discrete_proxy(unsigned int n, unsigned int c) const
{
	if (n<this->proxysets.size())
		if (c<this->proxysets[n].size())
				return this->proxysets[n][c];
	return NULL;
}

template<typename T> std::vector<T*> DiscreteProxySolution<T>::discrete_proxies(unsigned int n) const
{
	if (n<this->proxysets.size())
			return this->proxysets[n];
	return std::vector<T*>();
}

}

#endif
