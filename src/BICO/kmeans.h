#ifndef KMEANS_H_
#define KMEANS_H_

#include "master.h"
#include "triple.h"
#include <limits>
#include <math.h>
#include <algorithm>
#include <random>
#include "randomness.h"
#include <iostream>

class MASTER;

class KMEANS
{
public:
    KMEANS(MASTER*master, int k, int dimension, int numberOfPoints, bool projections);
    KMEANS(MASTER*master, int k, int dimension, int numberOfPoints);
    bool reGroupPoints(triple<double, double*, int>** points, double** centers, double *weights);
    int findNearest(double* point, double** centers);
    int findNearest(triple<double, double*, int>* point, double** centers, double* projection, double projectVal);
    double**recomputeCenters(triple<double, double*, int>** points);
    void initialCenters(triple<double, double*, int>** points, double** centers);
    double squaredDistance(double*p1, double*p2);
private:
    bool projections;
    int k;
    int dimension;
    int numberOfPoints;
    MASTER*master;

    std::random_device rd;
    std::mt19937 rg;
};

KMEANS::KMEANS(MASTER*master, int k, int dimension, int numberOfPoints) : rg(rd())
{

    this->master = master;
    this->k = k;
    this->dimension = dimension;
    this->numberOfPoints = numberOfPoints;
    this->projections = false;
}

KMEANS::KMEANS(MASTER*master, int k, int dimension, int numberOfPoints, bool projections) : rg(rd())
{
    this->master = master;
    this->k = k;
    this->dimension = dimension;
    this->numberOfPoints = numberOfPoints;
    this->projections = projections;
}

bool KMEANS::reGroupPoints(triple<double, double*, int>**points, double**centers, double* weight)
{
    if (projections && (points[numberOfPoints - 1]->third != -1))
    {

        double projection[dimension];
        double random;
        double length = 0;
        for (int i = 0; i < dimension; i++)
        {
            std::normal_distribution<double> realDist(0.0, 1.0);
            random = realDist(rg);
            projection[i] = random;
            length += random*random;
        }
        length = sqrt(length);
        for (int i = 0; i < dimension; i++)
        {
            projection[i] = projection[i] / length;
        }
        double sum;
        for (int i = 0; i < k; i++)
        {
            sum = 0;
            for (int j = 0; j < dimension; j++)
            {
                sum += projection[j] * centers[i][j];
            }
        }
        bool result = true;
        double newCenter[k][dimension];
        for (int i = 0; i < k; i++)
        {
            weight[i] = 0;
            for (int j = 0; j < dimension; j++)
            {
                newCenter[i][j] = 0;
            }
        }
        int temp;
        double projectVal=0.0;

        //std::cout << "projection vektor created, grouping centers..."<<std::endl;
        //compute nearest center and store contribution to the new centroid
        for (int i = 0; i < numberOfPoints; i++)
        {
            for (int j = 0; j < dimension; j++)
            {
                projectVal += projection[j]* (points[i]->second[j]);
            }
            //std::cout << "Point projected"<<std::endl;
            temp = findNearest(points[i], centers, projection, projectVal);
            //std::cout << "Nearest center: " << temp << std::endl;
            if (temp != points[i]->third)
            {
                result = false;
                points[i]->third = temp;
            }
            weight[points[i]->third] += points[i]->first;
            for (int j = 0; j < dimension; j++)
            {
                newCenter[points[i]->third][j] += points[i]->second[j] * points[i]->first;
            }
            //std::cout<<"Neighbour "<< i <<" grouped to cluster "<<points[i]->third<<std::endl;
        }
        //compute new centroid
        for (int i = 0; i < k; i++)
        {
            for (int j = 0; j < dimension; j++)
            {
                centers[i][j] = newCenter[i][j] / weight[i];
            }
        }
        return result;
    }
    else
    {
        bool result = true;
        double newCenter[k][dimension];
        for (int i = 0; i < k; i++)
        {
            weight[i] = 0;
            for (int j = 0; j < dimension; j++)
            {
                newCenter[i][j] = 0;
            }
        }
        int temp;

        //compute nearest center and store contribution to the new centroid
        for (int i = 0; i < numberOfPoints; i++)
        {
            temp = findNearest(points[i]->second, centers);
            if (temp != points[i]->third)
            {
                result = false;
                points[i]->third = temp;
            }
            weight[points[i]->third] += points[i]->first;
            for (int j = 0; j < dimension; j++)
            {
                newCenter[points[i]->third][j] += points[i]->second[j] * points[i]->first;
            }
        }
        //compute new centroid
        for (int i = 0; i < k; i++)
        {
            for (int j = 0; j < dimension; j++)
            {
                centers[i][j] = newCenter[i][j] / weight[i];
            }
        }
        return result;
    }
}

int KMEANS::findNearest(triple<double, double*, int>* point, double** centers, double* projection, double projectVal)
{
    int nearest = point->third;
    //std::cout << "next center: "<<nearest<<std::endl;
    double minDistance = sqrt(squaredDistance(point->second, centers[point->third]));
    //std::cout << "Distance to current cluster: " <<minDistance<<std::endl;
    double distance;
    for (int i = 0; i < point->third; i++)
    {
        if (minDistance > fabs(projection[i] - projectVal))
        {
            distance = sqrt(squaredDistance(point->second, centers[i]));
            if (distance < minDistance)
            {
                minDistance = distance;
                nearest = i;
            }
        }
    }
    for (int i = point->third + 1; i < k; i++)
    {
        if (minDistance > fabs(projection[i] - projectVal))
        {
            distance = sqrt(squaredDistance(point->second, centers[i]));
            if (distance < minDistance)
            {
                minDistance = distance;
                nearest = i;
            }
        }
    }
    return nearest;
}

int KMEANS::findNearest(double* point, double** centers)
{
    int nearest = 0;
    double minDistance = std::numeric_limits<double>::max();
    double distance = 0;
    for (int i = 0; i < k; i++)
    {
        distance = squaredDistance(point, centers[i]);
        if (distance < minDistance)
        {
            minDistance = distance;
            nearest = i;
        }
    }
    return nearest;
}

void KMEANS::initialCenters(triple<double, double*, int>** points, double**centers)
{
    double randomR;
    double distance[numberOfPoints];
    int randomI;
    std::uniform_real_distribution<double> realDist(0.0, 1.0);
    randomR = realDist(rg);
    std::uniform_int_distribution<int> intDist(0, numberOfPoints - 1);
    randomI = intDist(rg);
    double temp;

    for (int i = 0; i < dimension; i++)
    {
        centers[0][i] = points[randomI]->second[i];
    }
    int candidate = 0;
    double sum = 0;

    for (int i = 0; i < k - 1; i++)
    {
        sum = 0;
        for (int j = 0; j < numberOfPoints; j++)
        {
            if (i == 0)
            {
                distance[j] = points[j]->first * squaredDistance(points[j]->second, centers[i]);
            }
            else
            {
                distance[j] = std::min(distance[j], points[j]->first * squaredDistance(points[j]->second, centers[i]));
            }
            sum += distance[j];
        }
        std::uniform_real_distribution<double> realDist(0.0, sum);
        randomR = realDist(rg);
        temp = distance[0];
        candidate = 0;
        while (!(randomR < temp))
        {
            temp += distance[++candidate];
        }
        for (int l = 0; l < dimension; l++)
        {
            centers[i + 1][l] = points[candidate]->second[l];
        }
    }
    /*
          std::cout << "initial centers:"<<std::endl;
          for(int i = 0; i < k; i++) {
                  for(int j = 0; j < dimension; j++) {
                          std::cout<<centers[i][j] << " ";
                  }
                  std::cout<<std::endl;
          }
     */
}

double KMEANS::squaredDistance(double*p1, double*p2)
{
    double distance = 0;
    double temp = 0;
    for (int j = 0; j < dimension; j++)
    {
        temp = p1[j] - p2[j];
        distance += temp*temp;
    }
    return distance;
}



#endif /* KMEANS_H_ */
