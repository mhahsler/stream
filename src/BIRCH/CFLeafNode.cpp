//
// Created by Dede on 16.09.2016.
//

#include "CFLeafNode.hpp"

CFLeafNode::CFLeafNode(int bf,int maxEntries):branchingFactor(bf),maxEntries(maxEntries),next(NULL),prev(NULL) {
  this->entries = new std::vector<CF::ClusteringFeature*>;
}

CFLeafNode::~CFLeafNode(){
  for (unsigned int i = 0; i < this->entries->size(); i++) {
  //  Rcpp::Rcout<<"delete CF Leaf"<<std::endl;
    delete ((*this->entries)[i]); // Calls ~object and deallocates *tmp[i]
  }
  //Rcpp::Rcout<<"deallocate by putting it on the stack"<<std::endl;
  delete(this->entries);
  std::vector<std::pair<CF::ClusteringFeature*,CF::CFNode*> > entries;
}

std::vector<CF::ClusteringFeature*>* CFLeafNode::getEntries() {
 return this->entries;
}

int CFLeafNode::getLength(){
  return this->entries->size();
}

int CFLeafNode::getMaxEntries(){
  return this->maxEntries;
}

CFLeafNode* CFLeafNode::getNext(){
  return this->next;
}
CFLeafNode* CFLeafNode::getPrev(){
  return this->prev;
}

void CFLeafNode::setNext(CFLeafNode* node){
  this->next = node;
}
void CFLeafNode::setPrev(CFLeafNode* node){
  this->prev  =node;
}

//Here we find the closest entry based on
CF::ClusteringFeature* CFLeafNode::findClosestEntry(CF::ClusteringFeature* cf){
//Get the distance to
  double distance=(*this->getEntries())[0]->getInterClusterMetric(cf);
CF::ClusteringFeature* closest=(*this->getEntries())[0];
for(unsigned int i =0;i<this->getEntries()->size();i++){
  if((*this->getEntries())[i]->getInterClusterMetric(cf)<distance){
    distance = (*this->getEntries())[i]->getInterClusterMetric(cf);
    closest = (*this->getEntries())[i];
  }
}
return closest;
}

CF::CFNode* CFLeafNode::findClosestEntry(){
  return NULL;
}

CF::CFNode* CFLeafNode::findFarthestEntry(){
  return NULL;
}
