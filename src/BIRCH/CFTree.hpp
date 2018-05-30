//
// Created by Dede on 08.09.2016.
//

#ifndef CFTREE_CFTREE_H
#define CFTREE_CFTREE_H

#include "CFNode.hpp"
#include "CFLeafNode.hpp"
#include "CFNonLeafNode.hpp"
#include "ClusteringFeature.hpp"

namespace CF{
    class CFTree {

    public:
        CFTree(double treshold, int branchingFactor, int maxLeafEntries, int maxMemory, float outlierThreshold);

        ~CFTree();

        std::pair<CF::ClusteringFeature*,CF::CFNode*>* insertCF(ClusteringFeature* feature,CF::CFNode *node);

        std::pair<CF::ClusteringFeature*,CF::CFNode*>* mergeLeafNodes(CFLeafNode* leaf);

        std::pair<CF::ClusteringFeature*,CF::CFNode*>* mergeNonLeafNodes(CFNonLeafNode *node);

        std::vector<CF::ClusteringFeature *>* getAllLeafCF(CF::CFNode *node);

        void insert(ClusteringFeature *feature,CF::CFNode *node);

        void rebuild(double treshold);

        void deleteTree(CFNode* node, int deleteLeafs);

        void resetTreeWithNewTreshold(double treshold);

        void removeOutliers(std::vector<CF::ClusteringFeature*>* cfs);

        void printTree(CF::CFNode *node);

        CFNode* getRoot();

        int getMaxLeafEntries();

        int getTreeHeight();

        int getBranchingFactor();

        int getUsedMem();

        double findNewTreshold(CFNode* node);

        double getTreshold();

    private:
        CFNode* root;
        double treshold;
        unsigned int branchingFactor;
        int maxLeafEntries;
        int treeHeight;
        int usedMem;
        int maxMemory;
        float outlierTreshold;
        std::vector<CF::ClusteringFeature*>* features;
        std::vector<CF::ClusteringFeature*>* outlier;
    };
}


#endif //CFTREE_CFTREE_H
