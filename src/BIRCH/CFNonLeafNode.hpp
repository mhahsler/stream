//
// Created by Dede on 16.09.2016.
//

#ifndef CFTREE_CFNONLEAFNODE_HPP
#define CFTREE_CFNONLEAFNODE_HPP

#include <utility>
#include "CFNode.hpp"
#include "ClusteringFeature.hpp"
#include "CFLeafNode.hpp"


class CFNonLeafNode: public CF::CFNode {
public:
    CFNonLeafNode(int bf,int maxEntries);
  ~CFNonLeafNode();
    std::vector<std::pair<CF::ClusteringFeature*,CF::CFNode*> >* getEntries();
    CFNode* findClosestEntry();
    std::pair<CF::ClusteringFeature*,CF::CFNode*>* findClosestChild(CF::ClusteringFeature* cf);
    CFNode* findFarthestEntry();
    void updateCF(std::pair<CF::ClusteringFeature*,CF::CFNode*>* entry);
    CF::ClusteringFeature getOverallCF();
    int getLength();
private:
    int branchingFactor;
    std::vector<std::pair<CF::ClusteringFeature*,CF::CFNode*> >* entries;
};


#endif //CFTREE_CFNONLEAFNODE_HPP
