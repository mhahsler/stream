//
// Created by Dede on 08.09.2016.
//

#include "CFTree.hpp"
namespace CF {

CFTree::CFTree(double treshold, int branchingFactor, int maxLeafEntries, int maxMemory, float outlierThreshold) : treshold(treshold), branchingFactor(branchingFactor),maxLeafEntries(maxLeafEntries),treeHeight(0),usedMem(0),maxMemory(maxMemory),outlierTreshold(outlierThreshold) {
 // Rcpp::Rcout<<"Constructor called"<<std::endl;
  this->root = NULL;
  this->outlier = new std::vector<CF::ClusteringFeature*>();
}

/**
 * Destructor
 */
CFTree::~CFTree(){
  if(this!=NULL){
  //Destructor of tree simply calls delete Tree method
    this->deleteTree(this->root, 1);
    // Rcpp::Rcout<<"Tree successfully deleted"<<std::endl;
    ;}
}

void CFTree::insert(ClusteringFeature *feature,CF::CFNode *node){
  std::pair<CF::ClusteringFeature*,CF::CFNode*>* result  = this->insertCF(feature,node);
  //We have to make a new root
  if(result!=NULL){

    // Here we create our future new root node which is a NonLeafNode
    CFNonLeafNode *newNode = new CFNonLeafNode(this->branchingFactor,this->maxLeafEntries);

    //Our old root still has some content after the split in lower level, so we have to create a entry for this old root
    std::pair<CF::ClusteringFeature*,CF::CFNode*>* oldRoot = new std::pair<CF::ClusteringFeature*,CF::CFNode*>;
    oldRoot->second=node;
    oldRoot->first = new CF::ClusteringFeature(result->first->getLs().size());

    //Rcpp::Rcout<<"Old root length: "<<oldRoot->second->getLength()<<std::endl;
    //Rcpp::Rcout<<"New length: "<<result->second->getLength()<<std::endl;

    //We push the old root and the new result to our newly created rootNode
    (*newNode->getEntries()).push_back(*oldRoot);
    (*newNode->getEntries()).push_back(*result);

    newNode->updateCF(oldRoot);
    newNode->updateCF(result);
    this->root= newNode;

    //Increase tree height
    this->treeHeight+=1;
  }

  //If we run out of memory, we have to rebuild
  if(this->getUsedMem()>this->maxMemory && maxMemory != 0){
    // CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(this->getRoot());

    //Rcpp::Rcout<<"Start rebuilding with new treshold "<<(this->findNewTreshold(this->getRoot()))<<std::endl;
   // this->printTree(this->root);
    this->rebuild((this->findNewTreshold(this->getRoot())));
  }

}

std::pair<CF::ClusteringFeature*,CF::CFNode*>* CFTree::insertCF(ClusteringFeature *feature,CF::CFNode *node) {
  //Case: If tree is currently empty. Insert LeafNode!
  if (this->root==NULL){
   // Rcpp::Rcout<<"Root is null, so create a new Leaf"<<std::endl;
    CFLeafNode* leaf = new CFLeafNode(this->branchingFactor,this->maxLeafEntries);
    leaf->getEntries()->push_back(feature);
    this->root = (leaf);
    return NULL;
  }
  // Else: If the current node is a leaf!
  else if(typeid(*node) == typeid(CFLeafNode)){

    //Here we cast from Node to LeafNode pointer
    CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);

    //Find closest clustering feature
    ClusteringFeature *entry = d->findClosestEntry(feature);


    //If the closest entry can absorb the new CF (treshold), update

    if(entry->canAbsorb(feature,false,this->getTreshold())){
     // Rcpp::Rcout<<"We can absorb it"<<std::endl;
      entry->add(feature);
      //Incoming feature deleted because it was merged before
      delete(feature);
      return NULL;
    }

    //Now we have two cases: 1. We can create a new entry (L) or we have to split the leaf node
    else{
     /* ClusteringFeature f(entry->getLs().size());
      f.add(feature);
      f.add(entry);*/

      //However in both cases we will insert the newly created CF. Therefore we have to update memory consumption
      this->usedMem+= sizeof(*feature);

      // First case: We can create a new entry
      if(this->maxLeafEntries>d->getLength()){
        d->getEntries()->push_back(feature);
        return NULL;
      }
      //Second case: we have to split the leaf node! Here we create a new one!
      else{
       // Rcpp::Rcout<<"We have to split!"<<std::endl;

        // We locally push it to our entries. We will split it afterwards so its okay.
        d->getEntries()->push_back(feature);

        //Here we split into two nodes. On the one hand we already have a node but we need two. So we split the current node according to the farthest criteria
        std::pair<CF::ClusteringFeature*,CF::CFNode*>* newEntry = this->mergeLeafNodes(d);


        //Here we set all next and prev pointers in order to maintain a chain of LeafNodes
        // CFLeafNode *newLeaf = dynamic_cast<CFLeafNode *>(newEntry->second);

        //References to the other leafs
       // newLeaf->setNext(d->getNext());

       // d->setNext(newLeaf);
       // newLeaf->setPrev(d);

        return newEntry;
      }
    }
  }

  //Case: We are not at a Leaf Node, so we have to go deeper
  else{
   // Rcpp::Rcout<<"Current node is a NON-leaf!"<<std::endl;
    CFNonLeafNode *nonNode = dynamic_cast<CFNonLeafNode *>(node);
    //We get the pair CF/Pointer to next node
    std::pair<CF::ClusteringFeature*,CF::CFNode*>* next = nonNode->findClosestChild(feature);
    //Go deeper into the tree
    std::pair<CF::ClusteringFeature*,CF::CFNode*>* result = this->insertCF(feature,next->second);

    //<---------------------------------------------------------------------------------------------------------->

    // Now we propagate back! Two cases: We have a split and have to propagate upwards or the split ended
    if(result==NULL){
      //update all CF of nonleafes (can I just add?)
      nonNode->updateCF(next);
      return NULL;
    }

    //We had a split. Now we have to check whether we have another nonleaf split or not!
    else{
      //We splitted the "next" child and created result, so we have to update the corresponding CF values!
      nonNode->updateCF(result);
      nonNode->updateCF(next);
      if(nonNode->getEntries()->size()<this->branchingFactor){
        nonNode->getEntries()->push_back(*result);
        return NULL;
      }
      //We have to split again!
      else{
        //Nontheless we have to add the entry and then split
        nonNode->getEntries()->push_back(*result);

      //  Rcpp::Rcout<<"We have again a split"<<std::endl;
        std::pair<CF::ClusteringFeature*,CF::CFNode*>* newEntry = this->mergeNonLeafNodes(nonNode);

        return newEntry;
      }

    }
    //Now we finally check whether the tree has to be reb
  }
}

std::pair<CF::ClusteringFeature*,CF::CFNode*>* CFTree::mergeLeafNodes(CFLeafNode* leaf){
  CFLeafNode *newLeaf = new CFLeafNode(this->branchingFactor,this->maxLeafEntries);
  CF::ClusteringFeature *c1=NULL;
  CF::ClusteringFeature *c2=NULL;
  std::vector<int> indexToDelete;

  double score=-1;
  int c2Pos = -1;
  //Rcpp::Rcout<<"Find farthest pair"<<std::endl;
  //First we have to find the farthest pair of entries
  //Rcpp::Rcout<<"Farthest pair begin ---------------------"<<std::endl;
  for(int i = 0;i<leaf->getLength()-1;i++){
    for(int j=i+1;j<leaf->getLength();j++){
     // Rcpp::Rcout<<"dist: "<<(*leaf->getEntries())[i]->getInterClusterMetric((*leaf->getEntries())[j])<<std::endl;
      if(score<(*leaf->getEntries())[i]->getInterClusterMetric((*leaf->getEntries())[j])){
        score=(*leaf->getEntries())[i]->getInterClusterMetric((*leaf->getEntries())[j]);
        c1 = (*leaf->getEntries())[i];
        c2 = (*leaf->getEntries())[j];
        c2Pos = j;
      }
    }
  }


  //Now we have to redistribute all entries between those two CF's
  for(int i = leaf->getLength()-1;i>=0;i--){
    // If the current entry is nearer to c2, add this one to the new leaf
    if((*leaf->getEntries())[i]->getInterClusterMetric(c1)>(*leaf->getEntries())[i]->getInterClusterMetric(c2) || i==c2Pos){
    //  Rcpp::Rcout<<"Push into c2: "<< (*leaf->getEntries())[i]->getCentroid()<<std::endl;
      (*newLeaf->getEntries()).push_back((*leaf->getEntries())[i]);
      indexToDelete.push_back(i);
    }
  }


  // Rcpp::Rcout<<"Delete after insertion"<<std::endl;
  //Dirty:::: In order to avoid index problems, we have a separate loop to delete after insertion
  for(unsigned int i = 0;i<indexToDelete.size();i++){
    // If the current entry is nearer to c2, add this one to the new leaf
    //Rcpp::Rcout<<"Delete index: "<<indexToDelete[i]<<std::endl;
   (*leaf->getEntries()).erase((*leaf->getEntries()).begin()+indexToDelete[i]);

  }


  //Now we have to create and fill the new NonLeaf Entry for the level above
  std::pair<CF::ClusteringFeature*,CF::CFNode*>* newEntry = new std::pair<CF::ClusteringFeature*,CF::CFNode*>();
  newEntry->second=newLeaf;
  newEntry->first = new CF::ClusteringFeature((*leaf->getEntries())[0]->getLs().size());

  //update memory used
  this->usedMem+= sizeof(*newEntry->first);

 // Rcpp::Rcout<<"Successfully merged"<<std::endl;
  return newEntry;

}

std::pair<CF::ClusteringFeature*,CF::CFNode*>* CFTree::mergeNonLeafNodes(CFNonLeafNode* node){
 // Rcpp::Rcout<<"Merge Non Leafs!"<<std::endl;
  std::vector<int> indexToDelete;
  CFNonLeafNode *newNonLeaf = new CFNonLeafNode(this->branchingFactor,this->maxLeafEntries);
  CF::ClusteringFeature *c1=NULL;
  CF::ClusteringFeature *c2=NULL;
  double score=-1;

  //First we have to find the farthest pair of entries
  for(int i = 0;i<node->getLength()-1;i++){
    for(int j=i+1;j<node->getLength();j++){
    //  Rcpp::Rcout<<"InterClusterMetric: "<<(*node->getEntries())[i].first->getInterClusterMetric((*node->getEntries())[j].first)<<std::endl;
      if(score<(*node->getEntries())[i].first->getInterClusterMetric((*node->getEntries())[j].first)){
        score=(*node->getEntries())[i].first->getInterClusterMetric((*node->getEntries())[j].first);
        c1 = (*node->getEntries())[i].first;
        c2 = (*node->getEntries())[j].first;
      }
    }
  }

  //Now we have to redistribute all entries between those two CF's
  for(int i = node->getLength()-1;i>=0;i--){
    // If the current entry is nearer to c2, add this one to the new leaf
    if((*node->getEntries())[i].first->getInterClusterMetric(c1)>(*node->getEntries())[i].first->getInterClusterMetric(c2)){
      (*newNonLeaf->getEntries()).push_back((*node->getEntries())[i]);
      indexToDelete.push_back(i);
    }

  }
  //Dirty:::: In order to avoid index problems, we have a separate loop to delete after insertion
  for(unsigned int i = 0;i<indexToDelete.size();i++){
    // If the current entry is nearer to c2, add this one to the new leaf
   // if((*node->getEntries())[i].first->getInterClusterMetric(c1)>(*node->getEntries())[i].first->getInterClusterMetric(c2))
      (*node->getEntries()).erase((*node->getEntries()).begin()+indexToDelete[i]);

  }

  //Now we have to create and fill the new NonLeaf Entry for the level above
  std::pair<CF::ClusteringFeature*,CF::CFNode*>* newEntry = new std::pair<CF::ClusteringFeature*,CF::CFNode*>();

  newEntry->second=newNonLeaf;
  newEntry->first = new CF::ClusteringFeature((*node->getEntries())[0].first->getLs().size());

  //Update memory consumption
  this->usedMem+= sizeof(*newEntry->first);

  //Rcpp::Rcout<<"Successfully merged!"<<std::endl;
  return newEntry;
}

void CFTree::deleteTree(CFNode* node, int deleteLeafs){
  // Rcpp::Rcout<<"Starting to delete"<<std::endl;
  if(typeid(*node) == typeid(CFNonLeafNode)){
    CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(node);
    for(int i=0;i<node->getLength();i++){
     deleteTree((*d->getEntries())[i].second, deleteLeafs);
    }
    delete(d);
   // Rcpp::Rcout<<"NonLeaf Node deleted"<<std::endl;
  }
  else if(deleteLeafs==1){
    CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
    delete(d);
   // Rcpp::Rcout<<"Leaf Node deleted"<<std::endl;
  }
  this->root = NULL;
}

void CFTree::resetTreeWithNewTreshold(double treshold){
  this->treshold=treshold;
  this->usedMem =0;
  this->treeHeight=0;
  this->root=NULL;
}

CFNode* CFTree::getRoot(){
  return this->root;
}

int CFTree::getTreeHeight(){
  return this->treeHeight;
}

int CFTree::getBranchingFactor(){
  return this->branchingFactor;
}

double CFTree::getTreshold(){
  return this->treshold;
}

int CFTree::getUsedMem(){
  return this->usedMem;
}

int CFTree::getMaxLeafEntries(){
  return this->maxLeafEntries;
}

void CFTree::printTree(CF::CFNode *node){
  if(typeid(*node) == typeid(CFNonLeafNode)){
    CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(node);
    Rcpp::Rcout<<"NonLeaveNode with "<<d->getLength()<<" entries and a overall LS of"<<d->getOverallCF().getLs()<<"and Centroid --> "<<d->getOverallCF().getCentroid()<<"and N -->" <<d->getOverallCF().getN() <<std::endl;
    for(int j=0;j<d->getLength();j++) {Rcpp::Rcout<<"Centroid: "<<(*d->getEntries())[j].first->getCentroid()<<",N: "<< (*d->getEntries())[j].first->getN()<<","<<"; ";}
    Rcpp::Rcout<<std::endl;
    for(int i=0;i<node->getLength();i++){
      printTree((*d->getEntries())[i].second);
    }
  }
  else{
    Rcpp::Rcout<<"Leave node with "<<node->getLength()<<" entries"<<std::endl;
    for(int j = 0; j<node->getLength();j++){
      CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
      Rcpp::Rcout<<"---------->Cluster Centroid: "<<(*d->getEntries())[j]->getCentroid()<<", SS: "<<(*d->getEntries())[j]->getSs()<<"N: "<<(*d->getEntries())[j]->getN()<<std::endl;

    }
  }
}

void CFTree::rebuild(double treshold){

  //First we get all leaf CF and
  std::vector<CF::ClusteringFeature*>* cfs = this->getAllLeafCF(this->getRoot());

  //Delete everything old
  this->deleteTree(this->getRoot(), 0);

  //Set everything else to zero
  this->resetTreeWithNewTreshold(treshold);


  //Remove outliers
  this->removeOutliers(cfs);

  //Insert into tree with new treshold
  for(unsigned int i = 0; i<(*cfs).size();i++){
   // Rcpp::Rcout<<"Inserting entry "<<std::endl;
    this->insert((*cfs)[i],this->getRoot());
  }
  //Rcpp::Rcout<<"Rebuilding done!!";
}

double CFTree::findNewTreshold(CFNode* node){
  if(typeid(*node) == typeid(CFNonLeafNode)){
    CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(node);
    int max = 0;
    for(int i=0;i<node->getLength();i++){
      if((*d->getEntries())[i].first->getN()>=max)
        max = i;
    }
    return findNewTreshold((*d->getEntries())[max].second);
  }
  else{
    if(node->getLength()>=2){
    //Rcpp::Rcout<<"current treshold: "<< this->getTreshold();
    CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
    double distance =(*d->getEntries())[0]->getInterClusterMetric((*d->getEntries())[1]);
    //For all entries we have to compare them with all other entries
    for(int j = 0; j<node->getLength()-1;j++){
      for(int k =1;k<node->getLength();k++){
        if(distance>(*d->getEntries())[j]->getInterClusterMetric((*d->getEntries())[k])){
          distance = (*d->getEntries())[j]->getInterClusterMetric((*d->getEntries())[k]);
        }
      }

    if(this->getTreshold()>=distance)
      return this->getTreshold()*2;
    else
      return distance;
    }
  }
  }
  return 0;
}

std::vector<CF::ClusteringFeature *>* CFTree::getAllLeafCF(CF::CFNode *node){
  if(this->root == node){
    features = new std::vector<CF::ClusteringFeature*>();
  }
   if(typeid(*node) == typeid(CFNonLeafNode)){
   CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(node);
   for(int i=0;i<node->getLength();i++){
    features=getAllLeafCF((*d->getEntries())[i].second);
   }
  }
   else{
     // CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
     for(int j = 0; j<node->getLength();j++){
      // Rcpp::Rcout<<"Push"<<std::endl;
       CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
       features->push_back((*d->getEntries())[j]);
     }
   }
   return features;
/*
  if(typeid(*node) == typeid(CFNonLeafNode)){
    CFNonLeafNode *d = dynamic_cast<CFNonLeafNode *>(node);
    features=getAllLeafCF((*d->getEntries())[0].second);
  }
  else{
    CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
    while(d!=NULL){
      Rcpp::Rcout<<"Leaf"<<std::endl;
      for(int j = 0; j<node->getLength();j++){
        Rcpp::Rcout<<"Push"<<std::endl;
        CFLeafNode *d = dynamic_cast<CFLeafNode *>(node);
        features->push_back((*d->getEntries())[j]);
      }
      d=d->getNext();
    }
  }

  return features;*/
}

void CFTree::removeOutliers(std::vector<CF::ClusteringFeature*>* cfs){
  float avg=0;
  //Here we calculate the average number of points per leaf entry
  for(unsigned int i = 0; i<cfs->size();i++){
    avg = avg+ (*cfs)[i]->getN();
  }
  avg= avg/cfs->size();
  //Now we delete all entries which are below anteil% of average number of points
  //and add them to our outliers
  unsigned int i =0;
 while(i<cfs->size()){
    if((*cfs)[i]->getN()<avg*this->outlierTreshold){
      this->outlier->push_back((*cfs)[i]);
      cfs->erase(cfs->begin()+i);
    }
    else{
      ++i;
    }
  }
}


}


