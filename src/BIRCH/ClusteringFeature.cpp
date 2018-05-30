//
// Created by Dede on 08.09.2016.
//

#include "ClusteringFeature.hpp"
#include "Rcpp.h"

namespace CF {

bool ClusteringFeature::canAbsorb(CF::ClusteringFeature *newCF,bool diameter, double treshold){
  ClusteringFeature f(newCF->getLs().size());
  f.add(newCF);

//  Rcpp::Rcout<<"This:"<<this->getLs()<<std::endl;
 // Rcpp::Rcout<<"This n:"<<this->getN()<<std::endl;
//  Rcpp::Rcout<<"This ss:"<<this->getSs()<<std::endl;
  f.add(this);
  //Rcpp::Rcout<<"InsertionCF Ls:"<<f.getLs()<<std::endl;
 // Rcpp::Rcout<<"InsertionCF n:"<<f.getN()<<std::endl;
 // Rcpp::Rcout<<"InsertionCF ss:"<<f.getSs()<<std::endl;
  if(diameter){
    Rcpp::Rcout<<"Diameter: "<<f.getDiameter()<<" and Treshold: "<<treshold<<std::endl;
    if(f.getDiameter()< treshold)
      return true;
    else return false;
  }
  else{
    //Rcpp::Rcout<<"Radius: "<<f.getRadius()<<" and Treshold: "<<treshold<<std::endl;
    if(f.getRadius()< treshold)
      return true;
      else return false;
  }

}

void ClusteringFeature::add(CF::ClusteringFeature *feature){
  this->n += feature->getN();
  this->ls += feature->getLs();
  this->ss += feature->getSs();
}

void ClusteringFeature::remove(CF::ClusteringFeature *feature){
  this->n -= feature->getN();
  this->ls = this->ls - feature->getLs();
  this->ss -= feature->getSs();
}

long ClusteringFeature::getN(){
  return this->n;
}

void ClusteringFeature::setN(long n) {
  this->n = n;
}

Rcpp::NumericVector &ClusteringFeature::getLs()  {
  return ls;
}

void ClusteringFeature::setLs(const  Rcpp::NumericVector &ls) {
  ClusteringFeature::ls = ls;
}

double ClusteringFeature::getSs(){
  return ss;
}

void ClusteringFeature::setSs(double ss) {
  ClusteringFeature::ss = ss;
}

ClusteringFeature::ClusteringFeature(ClusteringFeature *feature) {
  this->setN(feature->getN());
  this->setLs(feature->getLs());
  this->setSs(feature->getSs());
  this->interClusterMetric=0;
}


ClusteringFeature::ClusteringFeature(long n,  Rcpp::NumericVector ls,  double ss) {
  this->setN(n);
  this->setLs(ls);
  this->setSs(ss);
  //D0 is always the default metric
  this->interClusterMetric=0;
}

ClusteringFeature::ClusteringFeature(short dim){
  this->setN(0);
  this->setLs(Rcpp::NumericVector(dim,0.0));
  this->setSs(0);
  //D0 is always the default metric
  this->interClusterMetric=0;
}

ClusteringFeature::ClusteringFeature(Rcpp::NumericVector v){
  this->setN(1);
  this->setLs(v);
  this->setSs(Utility::Util::innerProduct(v,v));
  //D0 is always the default metric
  this->interClusterMetric=0;
}

Rcpp::NumericVector ClusteringFeature::getCentroid() {  Rcpp::NumericVector v;
  v = this->getLs() / this->getN();
  return v;
}

double ClusteringFeature::getRadius() {
  double result;
  double v = 2* Utility::Util::innerProduct(this->getCentroid(),this->getLs());
 double v2 = this->getN()* Utility::Util::innerProduct(this->getCentroid(),this->getCentroid());
  result = sqrt(((this->getSs() - v + v2)/this->getN()));
 // double v = this->getSs()/this->getN();
  //Rcpp::Rcout<<"v: "<<v;
  //Rcpp::NumericVector vec = this->getLs()/this->getN();


  //double skalar = Utility::Util::innerProduct(vec,vec);
  //Rcpp::Rcout<<"skalar: "<<skalar;
  //result = v-skalar;
  return result;
}

double ClusteringFeature::getDiameter() {
  return (2*(this->getN()*this->getSs()-Utility::Util::innerProduct(this->getLs(),this->getLs())));
}

double ClusteringFeature::getD0(CF::ClusteringFeature *c2) {
//  Rcpp::Rcout<<"GetD0"<<std::endl;
  Rcpp::NumericVector v;
  v= this->getCentroid()-c2->getCentroid();
  //Rcpp::Rcout<<"okay: "<<v<<std::endl;
 // Rcpp::Rcout<<sqrt(Utility::Util::innerProduct(v,v))<<std::endl;
  return sqrt(Utility::Util::innerProduct(v,v));
}

double ClusteringFeature::getD1(CF::ClusteringFeature *c2) {
  // Rcpp::Rcout<<"D1"<<std::endl;
  Rcpp::NumericVector v;
  v= this->getCentroid()-c2->getCentroid();
  return Utility::Util::vectorLengthManhatten(v);
}

double ClusteringFeature::getD2() {
  return 0;
}

double ClusteringFeature::getD3() {
  return 0;
}

double ClusteringFeature::getD4() {
  return 0;
}

double ClusteringFeature::getInterClusterMetric(ClusteringFeature *cf){
 switch(this->interClusterMetric){
    case 0 : return this->getD0(cf);
    case 1 : return this->getD1(cf);
 default: return this->getD0(cf);

 }
}

void ClusteringFeature::clearCF(){
  this->setN(0);
  this->setLs(Rcpp::NumericVector(this->getLs().size()));
  this->setSs(0);
}

}
