#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <sstream>
#include <fstream>
#include <random>
#include <ctime>
#include <time.h>

#include <boost/algorithm/string.hpp>

#include "BICO/l2metric.h"
#include "BICO/squaredl2metric.h"
#include "BICO/point.h"
#include "BICO/pointweightmodifier.h"
#include "BICO/bico.h"
#include "BICO/randomness.h"
#include "BICO/randomgenerator.h"
#include "BICO/proxysolution.h"
#include "BICO/pointcentroid.h"
#include "BICO/pointweightmodifier.h"
#include "BICO/realspaceprovider.h"

#include "BICO/master.h"
#include <stdio.h>
#include <stdlib.h>


// data structure adapted to Rcpp NumericMatrix and IntegerVector
MASTER::MASTER(Rcpp::NumericMatrix data, Rcpp::IntegerVector weight, int k, int dimension, int iterations, bool projections) :
  dimension(dimension),
  k(k),
  iterations(iterations),
  numberOfPoints(0),
  costs(std::numeric_limits<double>::max()),
  projections(projections)
{

  centers = new double*[k];
  finalcenters = new double*[k];
  for (int i = 0; i < k; i++)
  {
    centers[i] = new double[dimension];
    finalcenters[i] = new double[dimension];
  }

  int countD = 0;
  int countP = 0;

  numberOfPoints = data.nrow();
  points = new triple<double, double*, int>*[numberOfPoints];

  while (countP < numberOfPoints)
  {
    if (countD == 0)
    {
      points[countP] = new triple<double, double*, int>(weight[countD], new double[dimension], -1);
      countD++;
    }
    else if (countD < dimension)
    {
      points[countP]->second[countD - 1] = data(countP, countD-1);
      countD++;
    }
    else
    {
      points[countP]->second[dimension - 1] = data(countP, countD-1);
      countD = 0;
      countP++;
    }
  }

}










class BICO {
public:

  Rcpp::NumericMatrix micro;
  Rcpp::IntegerVector microWeight;
  Rcpp::NumericMatrix macro;
  Rcpp::IntegerVector macroWeight;
  Rcpp::IntegerVector assignment;

  int k;
  int space;
  int p;
  int iterations;
  int d;

  CluE::Bico<CluE::Point> *bico = NULL;
  CluE::SquaredL2Metric metric;
  CluE::PointWeightModifier modifier;
  bool upToDate;

  BICO(int k, int space, int p, int iterations){
    this->k = k;
    this->space=space;
    this->p=p;
    this->iterations=iterations;
    this->upToDate =false;
  }

  ~BICO(){
    delete this->bico;
  }


  void cluster(Rcpp::NumericMatrix data){

    // initialize
    if(bico==NULL){
      this->d = data.ncol();
      // parameter n is not used, therefore pass dummy 0 value
      this->bico = new CluE::Bico<CluE::Point>(this->d, 0, this->k, this->p, this->space, &this->metric, &this->modifier);
    }

    this->upToDate=false;

    time_t starttime;

    // Randomness::initialize(seed);
    time(&starttime);
    int n = data.nrow();

    for(int row=0; row < n; row++){
      std::vector<double> coords;
      coords.reserve(data.ncol());
      for (int i = 0; i < this->d; ++i)
        coords.push_back(data(row,i));
      CluE::Point p(coords);

      // Call BICO point update
      *bico << p;
    }

    // Retrieve coreset
    CluE::ProxySolution<CluE::Point>* sol = bico->compute();

    // Output coreset points
    Rcpp::NumericMatrix micro(sol->proxysets[0].size(), data.ncol());
    Rcpp::IntegerVector microWeight(sol->proxysets[0].size());
    for (size_t i = 0; i < sol->proxysets[0].size(); ++i)
    {
      // Output weight
      microWeight(i) = sol->proxysets[0][i].getWeight();
      // Output center of gravity
      for (size_t j = 0; j < sol->proxysets[0][i].dimension(); ++j)
      {
        micro(i,j) = sol->proxysets[0][i][j];
      }
    }

    this->micro = micro;
    this->microWeight = microWeight;
  }




  void recluster(){

    if(this->micro.nrow() == 0){
      return;
    }

    // run kmeans++
    bool projections = false;

    MASTER master = MASTER(this->micro, this->microWeight, this->k, this->d, this->iterations, projections);
    //double weights[k];
    double* weights = new double[k];

    double*** centers = master.run(weights);


    triple<double, double*, int>** points = master.getAssignment();
    Rcpp::IntegerVector assignment(this->micro.nrow());
    for(int i=0;i<this->micro.nrow();i++){
      assignment(i) = points[i]->third;
    }

    // create return matrix
    Rcpp::NumericMatrix macro(this->k, this->d);
    Rcpp::IntegerVector macroWeight(this->k);
    for (int i = 0; i < this->k; i++)
    {
      macroWeight[i]=weights[i];
      for (int j = 0; j < micro.ncol(); j++)
      {
        macro(i,j) = (*centers)[i][j];
      }
    }

    delete[] weights;

    this->macro = macro;
    this->macroWeight = macroWeight;
    this->assignment = assignment;
  }


  Rcpp::NumericMatrix get_microclusters(){
    return this->micro;
  }

  Rcpp::IntegerVector get_microweights(){
    return this->microWeight;
  }

  Rcpp::NumericMatrix get_macroclusters(){
    if(!this->upToDate){
      this->recluster();
      this->upToDate=true;
    }
    return this->macro;
  }

  Rcpp::IntegerVector get_macroweights(){
    if(!this->upToDate){
      this->recluster();
      this->upToDate=true;
    }
    return this->macroWeight;
  }

  Rcpp::IntegerVector microToMacro(){
    if(!this->upToDate){
      this->recluster();
      this->upToDate=true;
    }
    return this->assignment;
  }

};


// Expose fields and methods to R
RCPP_EXPOSED_CLASS(BICO)

  RCPP_MODULE(MOD_BICO){
    using namespace Rcpp;


    class_<BICO>("BICO")
      .constructor<int, int, int, int>()
      .field("k", &BICO::k)
      .field("space", &BICO::space)
      .field("p", &BICO::p)
      .field("iterations", &BICO::d)
      .field("d", &BICO::d)

    .method("get_microclusters", &BICO::get_microclusters)
    .method("get_microweights", &BICO::get_microweights)
    .method("get_macroclusters", &BICO::get_macroclusters)
    .method("get_macroweights", &BICO::get_macroweights)
    .method("cluster", &BICO::cluster)
    .method("recluster", &BICO::recluster)
    .method("microToMacro", &BICO::microToMacro)
    ;

  }
