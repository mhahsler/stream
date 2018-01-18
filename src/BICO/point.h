#ifndef POINT_H
#define POINT_H

#include <iostream>
#include <vector>

#include "weightedobject.h"

namespace CluE
{

/**
 * @brief Weighted point of arbitrary dimension.
 *
 * @ingroup pointrelated_classes
 */
class Point : public WeightedObject
{
public:
	/**
	 * @brief Constructs a weighted point.
	 */
	Point(size_t dimension = 0, double pointWeight = 1.0):coordinates(std::vector<double>(dimension)),weight(pointWeight)
	{
	}

	/**
	 * @brief Constructs a weighted point.
	 */
	Point(std::vector<double> coords, double pointWeight = 1.0):coordinates(coords),weight(pointWeight)
	{
	}

	/**
	 * @brief Constructs a point of gravity.
	 * @throw InvalidArgumentException [0] Can't consolidate points with different dimensions!
	 */
	Point(std::vector<Point*> const&);

	/**
	 * @brief Copy constructor
	 */
	Point(Point const& p):coordinates(p.coordinates),weight(p.weight)
	{
	}

	virtual ~Point()
	{
	}

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	Point& operator+=(Point const & x);

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	Point& operator-=(Point const & x);

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	Point operator+(Point const & x) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	Point operator-(Point const & x) const;

	/**
	 * Accesses one particular coordinate entry.
	 */
	double& operator[](size_t index)
	{
		return this->coordinates[index];
	}

	/**
	 * Returns one particular coordinate entry.
	 */
	double operator[](size_t index) const
	{
		return this->coordinates[index];
	}

	size_t dimension() const
	{
		return this->coordinates.size();
	}

	virtual double getWeight() const
	{
		return this->weight;
	}

	virtual void setWeight(double w)
	{
		this->weight = w;
	}

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	double squaredL1distance(Point const&) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	double l1distance(Point const&) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	double squaredL2distance(Point const&) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	double l2distance(Point const&) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	double lpdistance(Point const&, double p) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 */
	double squaredLpDistance(Point const&, double p) const;

	/**
	 * @throw InvalidArgumentException [0] Incompatible dimensions!
	 * @throw InvalidRuntimeConfigurationException [1] Point has coordinate <= 0.
	 */
	double kullbackleibler(Point const&) const;

private:
	std::vector<double> coordinates;
	double weight;
};

std::ostream& operator<<(std::ostream&, Point const&);

Point operator*(double scalar, Point const & vec);

/**
 * @throw InvalidArgumentException [0] Incompatible dimensions!
 */
double operator*(Point const & vec1, Point const & vec2);

}

#endif
