#include <Rcpp.h>
#include "BIRCH/CFTree.hpp"
using namespace Rcpp;


class BIRCH {

public:
  CF::CFTree *tree = NULL;

  //' @title CF-Tree creation
  //' @param threshold Specifies the threshold used to check whether a new datapoint can be absorbed or not.
  //' @param branching Specifies the branching factor (maximum amount of child nodes for a nonleaf node) of the CF-Tree.
  //' @param maxLeaf Specifies the maximum number of entries within a leaf node.
  //' @param maxMemory Specifies the memory limitation for the whole CFTree in bytes. Default is 0, indicating no memory restriction.
  //' @param outlierThreshold Specifies the threshold for identifying outliers when rebuilding the CF-Tree.
  BIRCH(double threshold, int branching, int maxLeafEntries, int maxMemory, float outlierThreshold){
    this->tree = new CF::CFTree(threshold,branching,maxLeafEntries,maxMemory,outlierThreshold);
  }

  void insert(NumericVector v){
    CF::ClusteringFeature* feature = new CF::ClusteringFeature(v);
    this->tree->insert(feature,this->tree->getRoot());
  }

  //' @title CF-Tree insertion
  //' @description All data objects of a matrix are rowwise inserted into the CF-Tree
  //' @param data Matrix of data objects.
  void udpateTree(NumericMatrix data){

    //For each datapoint we are going to call insert
    for(int i = 0 ;i<data.nrow();i++){
      CF::ClusteringFeature* feature = new CF::ClusteringFeature(data(i,_));
      this->tree->insert(feature,this->tree->getRoot());
    }
  }


  //' @title Centroids of micro clusters
  //' @description This function returns all micro clusters of a given CF-Tree.
  Rcpp::NumericMatrix getCentroids(){


    std::vector<CF::ClusteringFeature*>* clusteringFeatures =  this->tree->getAllLeafCF(this->tree->getRoot());

    Rcpp::NumericVector vector = (*clusteringFeatures)[0]->getCentroid();

    Rcpp::NumericMatrix m((*clusteringFeatures).size(),vector.size());

    //Transform to a list of CF and return it
    for(unsigned int i = 0; i<(*clusteringFeatures).size();i++){
      m.row(i)=(*clusteringFeatures)[i]->getCentroid();
    }

    return m;
  }


  //' @title Weights of micro clusters
  //' @description This function returns all micro cluster weights of a given CF-Tree.
  Rcpp::NumericVector getWeights(){

    std::vector<int> v(0);

    std::vector<CF::ClusteringFeature*>* clusteringFeatures =  this->tree->getAllLeafCF(this->tree->getRoot());
    for(unsigned int i = 0; i<(*clusteringFeatures).size();i++){
      v.push_back((*clusteringFeatures)[i]->getN());
    }

    return Rcpp::NumericVector(v.begin(),v.end());
  }





  //' @title Print CF-Tree
  //' @description A textual representation of a given CF-Tree
  void printTree(){
    this->tree->printTree(this->tree->getRoot());
  }


  //' @title CF-Tree threshold
  //' @description Returns the current threshold of a CF-Tree
  double getThreshold(){
    return this->tree->getThreshold();
  }

  //' @title Deletes the whole tree structure
  void deleteTree(){
    this->tree->deleteTree(this->tree->getRoot(), 1);
    Rcpp::Rcout<<"Tree finally deleted"<<std::endl;
  }

};










// Return class objects to R
RCPP_EXPOSED_CLASS(BIRCH)

  // Expose class members and methods to R
  RCPP_MODULE(MOD_BIRCH){
    using namespace Rcpp;


    class_<BIRCH>("BIRCH")
      .constructor<double, int, int, int, float>()
    .method("insert", &BIRCH::insert)
    .method("udpateTree", &BIRCH::udpateTree)
    .method("getCentroids", &BIRCH::getCentroids)
    .method("getWeights", &BIRCH::getWeights)
    .method("printTree", &BIRCH::printTree)
    .method("getThreshold", &BIRCH::getThreshold)
    .method("deleteTree", &BIRCH::deleteTree)
    ;


  }
