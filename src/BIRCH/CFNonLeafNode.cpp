//
// Created by Dede on 16.09.2016.
//

#include "CFNonLeafNode.hpp"

CFNonLeafNode::CFNonLeafNode(int bf,int maxEntries){
  this->entries = new std::vector<std::pair<CF::ClusteringFeature*,CF::CFNode*> >;
}

CFNonLeafNode::~CFNonLeafNode(){
 for (unsigned int i = 0; i < this->entries->size(); i++) {
   // Rcpp::Rcout<<"delete CF NONLeaf"<<std::endl;
    delete  ((*this->entries)[i].first); // Calls ~object and deallocates *tmp[i]
 }
 // Rcpp::Rcout<<"deallocate by putting it on the stack"<<std::endl;
  delete(this->entries);
  std::vector<std::pair<CF::ClusteringFeature*,CF::CFNode*> > entries;
}

std::vector<std::pair<CF::ClusteringFeature*,CF::CFNode*> >* CFNonLeafNode::getEntries() {
    return this->entries;
    }

CF::ClusteringFeature CFNonLeafNode::getOverallCF(){
  CF::ClusteringFeature f((*this->getEntries())[0].first->getLs().size());
  for(int i = 0;i<this->getLength();i++){
    f.add((*this->getEntries())[i].first);
  }
return f;
}

CF::CFNode* CFNonLeafNode::findClosestEntry(){
  return NULL;
}

std::pair<CF::ClusteringFeature*,CF::CFNode*>* CFNonLeafNode::findClosestChild(CF::ClusteringFeature* cf){
  double distance=(*this->getEntries())[0].first->getInterClusterMetric(cf);
  std::pair<CF::ClusteringFeature*,CF::CFNode*>* closest=(&(*this->getEntries())[0]);
  //Rcpp::Rcout<<this->getEntries()->size()<<std::endl;
  for(unsigned int i =0;i<this->getEntries()->size();i++){
   // Rcpp::Rcout<<"dist: "<<(*this->getEntries())[i].first->getInterClusterMetric(cf)<<std::endl;
    if((*this->getEntries())[i].first->getInterClusterMetric(cf)<distance){
      distance = (*this->getEntries())[i].first->getInterClusterMetric(cf);
      closest = &(*this->getEntries())[i];
    }
  //  Rcpp::Rcout<<"closest dist: "<<distance<<std::endl;
  }
  return closest;
}

/**
 * Here we update an entry in a nonleaf node. Means a pair of CF, pointer to next node. We look into the next node and summing up all CF there
 * in order to get the CF of the current node
 */
void CFNonLeafNode::updateCF(std::pair<CF::ClusteringFeature*,CF::CFNode*>* entry){
 // Rcpp::Rcout<<"Test"<<std::endl;
  //First clear the current CF
  entry->first->clearCF();
  //Rcpp::Rcout<<"Everythin cleared"<<std::endl;
  //if the next node is a leaf
  if(typeid(*entry->second) == typeid(CFLeafNode)){
    CFLeafNode *d = dynamic_cast<CFLeafNode *>(entry->second);
    for(int i =0;i<d->getLength();i++){
      entry->first->add((*d->getEntries())[i]);
    }
  }
  //if the next node is a nonleaf
  else{
    CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(entry->second);
    for(int i =0;i<d->getLength();i++){
      entry->first->add((*d->getEntries())[i].first);
    }
  }
}

int CFNonLeafNode::getLength(){
  return this->entries->size();
}


