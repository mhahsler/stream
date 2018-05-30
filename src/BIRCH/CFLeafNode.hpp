//
// Created by Dede on 16.09.2016.
//

#ifndef CFTREE_CFLEAFNODE_HPP
#define CFTREE_CFLEAFNODE_HPP
#include "CFNode.hpp"
#include "ClusteringFeature.hpp"

class CFLeafNode : public CF::CFNode {
public:
    std::vector<CF::ClusteringFeature*> *getEntries();
    CFLeafNode(int bf,int maxEntries);
    ~CFLeafNode();
    int getLength();
    int getMaxEntries();
    CFLeafNode* getNext();
    CFLeafNode* getPrev();
    void setNext(CFLeafNode* node);
    void setPrev(CFLeafNode* node);
    CF::ClusteringFeature* findClosestEntry(CF::ClusteringFeature*);
    bool canAbsorb(CF::ClusteringFeature*);
    CFNode* findClosestEntry();
    CFNode* findFarthestEntry();
private:
    int branchingFactor;
    std::vector<CF::ClusteringFeature*> *entries;
    int maxEntries;
    CFLeafNode* next;
    CFLeafNode* prev;
};


#endif //CFTREE_CFLEAFNODE_HPP
