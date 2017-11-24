#include "point.h"

#include "invalidargumentexception.h"
#include "invalidruntimeconfigurationexception.h"

#include <math.h>
#include <iostream>
#include <numeric>
#include <vector>
#include <algorithm>

using namespace CluE;

Point::Point(std::vector<Point*> const& v)
{
	size_t size = v.size();
	double weightSum = 0;
	if(size>0)
	{
		size_t dimension = v[0]->dimension();
		weightSum = v[0]->weight;
		for(size_t i=1; i < size; ++i)
		{
			weightSum+=v[i]->weight;
			if(v[i]->dimension()!=dimension)
				throw InvalidArgumentException(0, "Can't consolidate points with different dimensions!", "v");
		}

		for(size_t n=0; n < dimension; ++n)
		{
			double c = 0;
			for(size_t i=0; i < size; ++i)
			{
				c += v[i]->weight*(*v[i])[n];
			}
			c/=weightSum;
			this->coordinates.push_back(c);
		}

		this->weight = weightSum;
	}
	else
	{
		this->weight = 0;
	}
}

Point& Point::operator+=(Point const & x)
{
	size_t d = this->dimension();

	if(x.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "x");

	for(size_t i = 0; i < d; ++i)
		(*this)[i] += x[i];

	return *this;
}

Point& Point::operator-=(Point const & x)
{
	size_t d = this->dimension();

	if(x.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "x");

	for(size_t i = 0; i < d; ++i)
		(*this)[i] -= x[i];

	return *this;
}

Point Point::operator+(Point const & x) const
{
	return Point(*this) += x;
}

Point Point::operator-(Point const & x) const
{
	return Point(*this) -= x;
}

double Point::l1distance(Point const& p) const
{
	size_t d = this->dimension();
	if(p.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "p");

	double sum = 0;
	for(size_t i=0; i<d; ++i)
		sum += fabs(p[i]-(*this)[i]);

	return sum;
}

double Point::squaredL1distance(Point const& p) const
{
	double l1 = this->l1distance(p);
	return l1*l1;
}

double Point::squaredL2distance(Point const& p) const
{
	size_t d = this->dimension();
	if(p.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "p");

	double sum = 0;
	for(unsigned int i=0; i<d; ++i)
	{
		double delta = p[i]-(*this)[i];
		sum += delta*delta;
	}

	return sum;
}

double Point::l2distance(Point const& p) const
{
	return sqrt(this->squaredL2distance(p));
}

double Point::lpdistance(Point const& p, double exp) const
{
	size_t d = this->dimension();
	if(p.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "p");

	double sum = 0;
	for(size_t i=0; i<d; ++i)
	{
		double delta = p[i]-(*this)[i];
		sum += pow(delta, exp);
	}

	sum = pow(sum, 1/exp);

	return sum;
}

double Point::squaredLpDistance(Point const& p, double exp) const
{
	return pow(lpdistance(p, exp), 2);
}

double Point::kullbackleibler(Point const& p) const
{
	size_t d = this->dimension();
	if(p.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "p");

	std::vector<double> temp;
	std::vector<double> sortedX;
	std::vector<double> sortedY;
	for(size_t i=0; i<d; ++i)
	{
		double xi = (*this)[i];
		sortedX.push_back(xi);
		double yi = p[i];
		sortedY.push_back(yi);
		if(xi>.0)
		{
			if(yi>.0)
			{
				double r = xi/yi;
				if(r>.0)
					temp.push_back(xi*log(r));
			}
			else
				throw InvalidRuntimeConfigurationException(1, "Point has coordinate <= 0.");
		}
		else
			throw InvalidRuntimeConfigurationException(1, "Point has coordinate <= 0.");
	}

	// for numeric stability
	sort(temp.begin(), temp.end());
	sort(sortedX.begin(), sortedX.end());
	sort(sortedY.begin(), sortedY.end());

	return accumulate(temp.begin(), temp.end(), .0)
	       -accumulate(sortedX.begin(), sortedX.end(), .0)
	       +accumulate(sortedY.begin(), sortedY.end(), .0);
}

std::ostream& CluE::operator<<(std::ostream& os, Point const& p)
{
	size_t d = p.dimension();
	os << "(";
	for(unsigned int i=0; i<d; i++)
	{
		os << p[i];
		if(i < d-1)
			os << ", ";
	}
	os << ")";
	return os;
}

Point CluE::operator*(double scalar, Point const & vec)
{
	size_t d = vec.dimension();

	Point ret(vec);
	for(size_t i = 0; i < d; ++i)
		 ret[i] = scalar * vec[i];

	return ret;
}

double CluE::operator*(Point const & vec1, Point const & vec2)
{
	size_t d = vec1.dimension();

	if(vec2.dimension() != d)
		throw InvalidArgumentException(0, "Incompatible dimensions!", "vec1 / vec2");

	double ret = 0;
	for(size_t i = 0; i<d; ++i)
		ret += vec1[i] * vec2[i];

	return ret;
}
