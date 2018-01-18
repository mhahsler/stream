#ifndef PROXYEVALUATION_H
#define PROXYEVALUATION_H

#include "evaluation.h"
#include "proxyprovider.h"
#include "discreteproxyprovider.h"

#include <vector>

namespace CluE {

/**
 * @brief Abstract class for proxy-based evaluation algorithms.
 *
 * Evaluation algorithms requiring proxies for calculating the input's cost should derive from this class.
 *
 * @ingroup base_classes
 */
template<typename T> class ProxyEvaluation : virtual public Evaluation {
public:
	virtual ~ProxyEvaluation() {
	}

	/**
	 * @brief Calculates the cost related to the proxies based on the input.
	 * @note There are separate overloaded versions of this method for use with discrete proxies.
	 */
	virtual double proxycost(std::vector<T*> const &input, std::vector<T> const &proxies) const = 0;
	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const &input, ProxyProvider<T> const &proxies, unsigned int solutionIndex) const = 0;
	/**
	 * @brief Calculates the cost related to a single proxy chosen from the whole list, based on the input.
	 * @note There are separate overloaded versions of this method for use with discrete proxies.
	 */
	virtual double proxycost(std::vector<T*> const &input, std::vector<T> const &proxies, unsigned int proxyIndex) const = 0;
	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const& input, ProxyProvider<T> const &proxies, unsigned int solutionIndex, unsigned int proxyIndex) const = 0;

	/**
	 * @brief Calculates the cost related to the discrete proxies based on the input.
	 */
	virtual double proxycost(std::vector<T*> const &input, std::vector<T*> const &proxies) const = 0;
	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const &input, DiscreteProxyProvider<T> const &proxies, unsigned int solutionIndex) const = 0;
	/**
	 * @brief Calculates the cost related to a single discrete proxy chosen from the whole list, based on the input.
	 */
	virtual double proxycost(std::vector<T*> const& input, std::vector<T*> const &proxies, unsigned int solutionIndex) const = 0;
	/**
	 * @overload
	 */
	virtual double proxycost(std::vector<T*> const& input, DiscreteProxyProvider<T> const &proxies, unsigned int solutionIndex, unsigned int proxyIndex) const = 0;
};

}

#endif
