#include "SigmaIndexProxy.hpp"

/*
 Abstract population exception.
 @param msg Exception message
 */
AbstractPopulation_Exception::AbstractPopulation_Exception(const char *msg) {
    this->msg=msg;
}
const char *AbstractPopulation_Exception::what() const throw() {
    return this->msg;
}

/**
 A method for the Mahalanobis distance calculation
 @param newElement Data point.
 @return Returns the Mahalanobis distance from the centroid to the input data point, using the population covariance matrix.
 */
double ManualPopulation::mahalanobisDistance(VectorXd *newElement) {
    if(newElement==NULL) throw AbstractPopulation_Exception("New element must be defined");
    if(newElement->size()!=mean->size()) throw AbstractPopulation_Exception("New element does not have the same dimension as the component");
    VectorXd delta=*newElement-*mean; // calculate the Mahalanobis distance
    double N0=delta.transpose()*(*icovariance*delta);
    return sqrtf(N0);
}

/**
 Manual population constructor.
 @param id A population identifier.
 @param mean A population centroid.
 @param covariance A non-inverted covariance matrix.
 @param elements Population size.
 @param updateCallback An update callback that is invoked when the population data changes.
 */
ManualPopulation::ManualPopulation(string id, VectorXd *mean, MatrixXd *covariance, long elements, function<void (string)> updateCallback) {
    FullPivLU<MatrixXd> lu(*covariance);
    if(lu.isInvertible()) { // check if the covariance matrix is invertible
        this->icovariance=new MatrixXd(covariance->inverse()); // invert it - this way the Mahalanobis distance calculation is less expensive
    } else throw AbstractPopulation_Exception("The given covariance matrix is not invertible");
    this->mean=new VectorXd(*mean); // set private properties
    this->elements=elements;
    this->id=id;
    this->updateCallback=updateCallback;
}

/**
 Manual population destructor.
 */
ManualPopulation::~ManualPopulation() {
    delete mean;
    delete icovariance;
}

/**
 Returns the size of the population.
 @returns The size of the population.
 */
long ManualPopulation::getElements() {
    return this->elements;
}

/**
 Update the covariance matrix for this population. The new covariance matrix must be also invertible.
 @param covariance A new covariance matrix.
 @param updateSI A flag whether to update Sigma-Index DAG.
 @note From the Sigma-Index point of view, this change is a change is the population size or shape.
 */
void ManualPopulation::updateCovariance(MatrixXd *covariance, bool updateSI) {
    FullPivLU<MatrixXd> lu(*covariance); // again check if the supplied cov. matrix is invertible
    if(!lu.isInvertible()) throw AbstractPopulation_Exception("The given covariance matrix is not invertible");
    this->icovariance=new MatrixXd(covariance->inverse()); // invert it
    if(updateCallback && updateSI) updateCallback(id); // notify the parent Sigma-Index that population has been changed
}

/**
 Update the centroid for this population.
 @param mean A new centroid.
 @param updateSI A flag whether to update Sigma-Index DAG.
 @note From the Sigma-Index point of view, this change is a population drift.
 */
void ManualPopulation::updateMean(VectorXd *mean, bool updateSI) {
    this->mean=new VectorXd(*mean);
    if(updateCallback && updateSI) updateCallback(id); // notify the parent Sigma-Index that population has been changed
}

/**
 Update the population size for this population.
 @param elements A new number of population elements.
 @param updateSI A flag whether to update Sigma-Index DAG.
 @note From the Sigma-Index point of view, this change might influence that structure of the DAG. If this population became bigger that any of the parents, or smaller that any of the children, the Sigma-Index DAG is not correct anymore and must be updated.
 */
void ManualPopulation::updateElements(long elements, bool updateSI) {
    this->elements=elements;
    if(updateCallback && updateSI) updateCallback(id); // notify the parent Sigma-Index that population has been changed
}

/**
 Returns the population centroid.
 @return Population centroid.
 */
VectorXd *ManualPopulation::getMean() {
    return this->mean;
}

/**
 Returns the population inverted covariance matrix.
 @return Inverted covariance matrix.
 */
MatrixXd *ManualPopulation::getICovariance() {
    return this->icovariance;
}

/**
 Can be used for cloning of this population
 @return Cloned population object.
 */
ManualPopulation *ManualPopulation::clone() {
    ManualPopulation *new_pop=new ManualPopulation();
    new_pop->id=id;
    new_pop->mean=new VectorXd(*mean);
    new_pop->icovariance=new MatrixXd(*icovariance);
    new_pop->elements=elements;
    new_pop->updateCallback=updateCallback;
    return new_pop;
}

/**
 Returns the population identifier.
 @return Population identifier.
 */
string ManualPopulation::getId() {
    return this->id;
}

/**
 A method that updates the population with new data point.
 @param data_point A new data point.
 @param updateSI A flag whether to update Sigma-Index DAG.
 */
void ManualPopulation::addNewElement(VectorXd *data_point, bool updateSI) {
    // calculate new mean
    VectorXd *old_mean=mean;
    VectorXd t1=(*data_point-*old_mean)/(elements+1);
    mean=new VectorXd(*old_mean+t1);
    // calculate new covariance
    // Sherman–Morrison–Woodbury incremental update is done here
    MatrixXd *old_cov=icovariance;
    MatrixXd u=(*data_point-*old_mean)/sqrt(elements);
    MatrixXd vt=((*data_point-*mean)/sqrt(elements)).transpose();
    MatrixXd v1=*old_cov*u*vt**old_cov;
    MatrixXd v2=vt**old_cov*u;
    MatrixXd res=*old_cov-v1/((double)1+v2(0,0));
    double X=(double)elements/((double)elements+(double)1);
    icovariance=new MatrixXd(res/X);
    elements++;
    if(updateCallback && updateSI) updateCallback(id);
}

