#ifndef SigmaIndexProxy_hpp
#define SigmaIndexProxy_hpp

#include <Eigen/Dense>
#include <string>
#include <set>
#include <vector>
#include <functional>
#include <iostream>
#include <random>
using namespace Eigen;
using namespace std;

/*
 Population main exception
 */
class AbstractPopulation_Exception : public exception {
private:
    const char *msg=NULL;
public:
    const char *what() const throw();
    AbstractPopulation_Exception(const char *msg);
};

/**
An abstract class for all populations. Contains virtual functions needed by the Sigma-Index code.
*/
class AbstractPopulation {
public:
    /**
     An abstract method for calculating the mahalanobis distance for the population. Must be implemented in the specialization class.
     @param newElement Data point whose distance is calculated
     @return The distance value
     */
    virtual double mahalanobisDistance(VectorXd *newElement) = 0;
    /**
     An abstract method for retrieving the population size.
     @return The number of population elements
     */
    virtual long getElements()=0;
    /**
     An abstract method for retrieving centroid.
     @return The centroid.
     */
    virtual VectorXd *getMean() {return NULL;}
    /**
     An abstract method for retrieving population identifier.
     @return The population identifier.
     */
    virtual string getId() {throw AbstractPopulation_Exception("Virtual function");}
};

/**
 A manual population class. Can be used to add population details to the Sigma-Index
 */
class ManualPopulation : public AbstractPopulation {
private:
    string id; // population identifier
    VectorXd *mean=NULL; // centroid
    MatrixXd *icovariance=NULL; // inverted covariance
    long elements=0; // the number of population elements
    function<void (string)> updateCallback=NULL; // update callback, which is invoked in case of the population update
    ManualPopulation()=default; // default constructor
public:
    ManualPopulation(string id, VectorXd *mean, MatrixXd *covariance, long elements, function<void (string)> updateCallback);
    ~ManualPopulation();
    double mahalanobisDistance(VectorXd *newElement);
    long getElements();
    VectorXd *getMean();
    MatrixXd *getICovariance();
    void updateCovariance(MatrixXd *covariance, bool updateSI=true);
    void updateMean(VectorXd *mean, bool updateSI=true);
    void updateElements(long elements, bool updateSI=true);
    void addNewElement(VectorXd *data_point, bool updateSI=true);
    ManualPopulation *clone();
    string getId();
};

#endif /* SigmaIndexProxy_hpp */
