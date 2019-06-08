#ifndef BICO_H
#define BICO_H

#include <array>
#include <cmath>
#include <memory>
#include <random>
#include <vector>


#include "streamingalgorithm.h"
#include "dissimilaritymeasure.h"
#include "solutionprovider.h"
#include "weightmodifier.h"
#include "partitionprovider.h"
#include "cfrentry.h"
#include "proxysolution.h"
#include "kmeansevaluator.h"
//#include "invalidruntimeconfigurationexception.h"
// We use the R random number generator
//#include "randomness.h"
#include <Rcpp.h>

namespace CluE
{

/**
 * @brief Fast computation of k-means coresets in a data stream
 *
 * BICO maintains a tree which is inspired by the clustering tree of BIRCH,
 * a SIGMOD Test of Time award-winning clustering algorithm.
 * Each node in the tree represents a subset of these points. Instead of
 * storing all points as individual objects, only the number of points,
 * the sum and the squared sum of the subset's points are stored as key features
 * of each subset. Points are inserted into exactly one node.
 * A detailed description of BICO can be found here:
 * * Hendrik Fichtenberger, Marc Gillé, Melanie Schmidt, Chris Schwiegelshohn,
 *   Christian Sohler: BICO: BIRCH Meets Coresets for k-Means Clustering.
 *   ESA 2013: 481-492
 *
 * In this implementation, the nearest neighbour search on the first level
 * of the tree ist sped up by projecting all points to random 1-d subspaces.
 * The first estimation of the optimal clustering cost is computed in a
 * buffer phase at the beginning of the algorithm.
 */
template<typename T> class Bico : public StreamingAlgorithm<T>
{
private:

    /**
     * @brief Class representing a node in BICO's tree
     */
    class BicoNode
    {
    public:
        typedef std::pair<CFREntry<T>, BicoNode*> FeaturePair;
        typedef std::list<FeaturePair> FeatureList;

        /**
         * Constructs a node for BICO's tree
         * @param outer Parent BICO instance
         */
        BicoNode(Bico<T>& outer) :
        objectId(outer.nodeIdCounter),
        outer(outer),
        features()
        {
            ++outer.nodeIdCounter;
        }

        /**
         * @brief Delete all nodes
         */
        void clear()
        {
            for (auto it = features.begin(); it != features.end(); ++it)
                delete it->second;
        }

        /**
         * Inserts a CFREntry into this node
         * @param feature CFREntry to be inserted
         * @return Iterator pointing to inserted CFREntry
         */
        typename FeatureList::iterator insert(CFREntry<T> const & feature)
        {
            return features.insert(features.end(),
                                   FeaturePair(feature, new BicoNode(outer)));
        }

        /**
         * Iterator pointing at the first CFREntry
         * @return Begin iterator
         */
        typename FeatureList::iterator begin()
        {
            return features.begin();
        }

        /**
         * Iterator pointing behind the last CFREntry
         * @return End iterator
         */
        typename FeatureList::iterator end()
        {
            return features.end();
        }

        /**
         * Number of contained CFREntries
         * @return Number of elements
         */
        size_t size()
        {
            return features.size();
        }

        /**
         * Indicates if node is empty
         * @return Indicator
         */
        bool empty()
        {
            return features.empty();
        }

        /**
         * Returns an iterator to the CFREntry in this node whose reference point
         * is nearest to a fixed point
         * @param element Fixed point
         * @param level Level of this node
         * @return Nearest CFREntry
         */
        typename FeatureList::iterator nearest(T const & element, int level)
        {
            typename FeatureList::iterator minIt = features.end();
            // Nearest neighbour search based on projections in level 1
            if (level == 1)
            {
                // Project point and calculate projection bucket number
                double val = outer.project(element, 0);
                int bucket_number = outer.calcBucketNumber(0, val);
                int mini = 0;
                int bucket_min = bucket_number;
                int mins;

                if ((bucket_number < 0) || (bucket_number > (int)outer.buckets[0].size() - 1))
                {
                    // The bucket does not exist (yet)
                    mins = 0;
                }
                else
                {
                    // Search for the projection with smallest bucket size
                    mins = outer.buckets[mini][bucket_min].size();
                    for (unsigned int i = 1; i < outer.L; i++)
                    {
                        val = outer.project(element, i);
                        bucket_number = outer.calcBucketNumber(i, val);
                        if ((bucket_number >= 0) & (bucket_number <= (int)outer.buckets[i].size() - 1))
                        {
                            int s = outer.buckets[i][bucket_number].size();
                            if (s < mins)
                            {
                                mins = s;
                                bucket_min = bucket_number;
                                mini = i;
                            }
                        }
                        else
                        {
                            mins = 0;
                            bucket_min = bucket_number;
                            mini = i;
                            break;
                        }
                    }

                }

                bucket_number = bucket_min;
                int rnd = mini;

                if (bucket_number < 0)
                {
                    // Bucket does not exist => create one
                    outer.allocateBucket(rnd, true);
                }
                else if (bucket_number > (int)outer.buckets[rnd].size() - 1)
                {
                    // Bucket does not exist => create one
                    outer.allocateBucket(rnd, false);
                }
                else
                {
                    // Bucket does exist => search nearest point in bucket
                    double minDist = -1;

                    for (auto it = outer.buckets[rnd][bucket_number].begin(); it != outer.buckets[rnd][bucket_number].end(); ++it)
                    {
                        double tmpDist = outer.measure->dissimilarity((*it)->first.representative, element);
                        if (tmpDist < minDist || minDist == -1)
                        {
                            minDist = tmpDist;
                            minIt = (*it);
                        }
                    }

                }
            }
                // Simple nearest neighbour search in all other levels
            else
            {
                double minDist = -1;
                for (auto it = features.begin(); it != features.end(); ++it)
                {
                    double tmpDist = outer.measure->dissimilarity(it->first.representative, element);
                    if (tmpDist < minDist || minDist == -1)
                    {
                        minDist = tmpDist;
                        minIt = it;
                    }
                }
            }

            return minIt;
        }

        /**
         * Removes a specified CFREntry
         * @param pos Position of the CFREntry to be removed
         */
        void erase(typename FeatureList::iterator pos)
        {
            features.erase(pos);
        }

        /**
         * Inserts all CFREntries of this node into a given FeatureList
         * @param to Destination of insertion
         * @param pos Position of insertion
         */
        void spliceAllTo(BicoNode* to, typename FeatureList::iterator pos)
        {
            to->features.splice(pos, features);
        }

        /**
         * Inserts one CFREntry of this node into a given FeatureList
         * @param it CFREntry to be inserted
         * @param to Destination of insertion
         * @param pos Postion of insertion
         */
        void spliceElementTo(typename FeatureList::iterator it, BicoNode* to, typename FeatureList::iterator pos)
        {
            to->features.splice(pos, features, it);
        }

        /**
         * Returns the unique object id
         * @return Object id
         */
        int id()
        {
            return objectId;
        }

    private:
        /**
         * @brief Unique object id
         */
        int objectId;

        /**
         * @brief Parent BICO instance
         */
        Bico<T>& outer;

        /**
         * List of all contained CFREntries
         */
        FeatureList features;
    };

public:
    /**
     * @brief Constructs BICO for points of type T
     * T can be an arbitrary type but it has to fulfil the requirements
     * of CFREntry.
     *
     * @param dimension Dimension of the data
     * @param n Size of the data
     * @param k Number of desired centeres
     * @param p Number of random projections used for nearest neighbour search
     *          in the first level
     * @param nMax Maximum coreset size
     * @param measure Implementation of the squared L2 metric for T
     * @param weightModifier Class to read and modify weight of T
     */
    Bico(size_t dimension, size_t n, size_t k, size_t p, size_t nMax,
         DissimilarityMeasure<T>* measure, WeightModifier<T>* weightModifier);

    /**
     * @brief Returns a coreset of all point read so far
     * @return Coreset
     */
    virtual ProxySolution<T>* compute();

    /**
     * @brief Read a point
     * Insert the point into BICO's tree
     *
     * @param element Point of type T
     * @return This BICO instance
     */
    virtual Bico<T>& operator<<(T const & element);

    /**
     * @brief Write the tree as GraphViz source into a stream
     * @param os Output stream
     */
    void print(std::ostream& os);

private:
   /**
   * @brief Inserts an element into a BicoNode at a certain level
   * @param node BicoNode to be inserted into
   * @param level Level of this BicoNode
   * @param element Elemente to be inserted
   */
  void insert(BicoNode* node, int level, T const & element);

  /**
  * @brief Inserts an element into the nearest neighbour data structure
  * @param iteratorElement Feature to be insertet into NN data structure
  */
  void insertIntoNN(typename BicoNode::FeatureList::iterator iteratorElement);

  /**
  * @brief Initialize nearest neighbour data structure
  */
  void initializeNN();

  /**
  * @brief Allocates a new bucket
  * @param bucket Number of projection
  * @param left Push front bucket (instead of push back)
  */
  void allocateBucket(int bucket, bool left);

  /**
  * Calculates the bucket number for a given value
  * @param rnd Number of projections
  * @param val Value
  * @return Bucket number
  */
  int calcBucketNumber(int rnd, double val);

  /**
  * @brief Projects a point onto a projection line
  * @param point Point
  * @param i Number of projection line
  * @return Projected point
  */
  double project(T point, int i);

  /**
  * @brief Rebuilds the tree
  */
  void rebuild();

  /**
  * Rebuilds the first level
  * @param parent New root
  * @param child Old root
  */
  void rebuildFirstLevel(BicoNode* parent, BicoNode* child);

  /**
  * Recursive rebuilding of the tree
  * @param node Some node to be rebuilded
  * @param level Level of this node
  */
  void rebuildTraversMerge(BicoNode* node, int level);

  /**
  * @brief Recursive computation of the coreset
  * @param node Some node to be processed
  * @param solution ProxySolution containing the coreset
  */
  void computeTraverse(BicoNode* node, ProxySolution<T>* solution);

  /**
  * @brief Returns the threshold for a given level
  * @param level Level
  * @return Threshold at this level
  */
  double getT(int level);

  /**
  * @brief Returns the radius for a given level
  * @param level Level
  * @return Radius at this level
  */
  double getR(int level);

  /**
  * Writes a BicoNode as GraphViz source into a stream
  * @param os Output stream
  * @param node Some BicoNode
  */
  void print(std::ostream& os, BicoNode* node);


  /**
  * @brief Random projection vectors
  */
  std::vector<std::vector<double >> rndprojections;

  /**
  * @brief Buckets for nearest neighbour search in first level
  */
  std::vector<std::vector<std::vector<typename BicoNode::FeatureList::iterator >> > buckets;
  /**
  * @brief Bucket borders
  */
  std::vector<std::pair<double, double >> borders;
  /**
  * @brief Width of buckets
  */
  std::vector<double> bucket_radius;

  /**
  * @brief Counter for unique BicoNode object ids
  */
  int nodeIdCounter;

  /**
  * @brief Buffer for DissimilarityMeasure
  */
  std::unique_ptr<DissimilarityMeasure<T >> measure;

  /**
  * @brief Buffer for WeightModifier
  */
  std::unique_ptr<WeightModifier<T >> weightModifier;

  /**
  * @brief Maximum coreset size
  */
  size_t maxNumOfCFs;

  /**
  * @brief Current coreset size
  */
  size_t curNumOfCFs;

  /**
  * @brief Number of centers
  */
  size_t k;

  /**
  * @brief Number of projections
  */
  size_t L;

  /**
  * @brief Current estimation of the optimal clustering cost
  */
  double optEst;

  /**
  * @brief Root node of BICO's tree
  */
  BicoNode* root;

  /**
  * @brief Buffer phase indicator
  */
  bool bufferPhase;

  /**
  * @brief Current number of rebuilding
  */
  int numOfRebuilds;

  /**
  * @brief Buffer phase's buffer
  */
  std::vector<T> buffer;

  /**
  * @brief Buffer phase's buffer for projected buffer points
  */
  std::vector<std::pair<double, T const *>> projection_buffer;

  /**
  * @brief Minimum pair distance of two points read in buffer phase
  */
  double minDist;

  /**
  * @brief Number of unique elements read in buffer phase
  */
  size_t pairwise_different;

  /**
  * @brief Dimension of the input points
  */
  size_t dimension;

  /**
  * @brief Extreme values used for constructing the nearest neighbour buckets
  */
  std::vector<double> maxVal;
};

template<template <typename> class P = std::less> struct comparePairFirst
{
    template<class T1, class T2> bool operator()(std::pair<T1, T2> const & left, std::pair<T1, T2> const & right)
    {
        return P<T1>()(left.first, right.first);
    }
};

template<typename T> Bico<T>::Bico(size_t dim, size_t n, size_t k, size_t p, size_t nMax,
                                   DissimilarityMeasure<T>* measure, WeightModifier<T>* weightModifier) :
nodeIdCounter(0),
measure(measure->clone()),
weightModifier(weightModifier->clone()),
maxNumOfCFs(nMax),
curNumOfCFs(0),
k(k),
L(p),
optEst(-1),
root(new BicoNode(*this)),
bufferPhase(true),
numOfRebuilds(0),
buffer(),
projection_buffer(),
minDist(std::numeric_limits<double>::infinity()),
pairwise_different(0),
dimension(dim)
{

    // RandomGenerator rg = Randomness::getRandomGenerator()

    // Create random device and generator
    //std::random_device rd;
    //std::mt19937 rg(rd());

    std::vector<double> rndpoint(dimension);
    rndprojections.resize(L);
    bucket_radius.resize(L);
    maxVal.resize(L);
    double norm;
    //std::normal_distribution<double> realDist(0.0, 1.0);
    for (unsigned int i = 0; i < L; i++)
    {
        maxVal[i] = -1;
        norm = 0.0;
        for (unsigned int j = 0; j < dimension; j++)
        {
            //rndpoint[j] = realDist(rg);
            rndpoint[j] = R::rnorm(0,1);
            norm += rndpoint[j] * rndpoint[j];
        }
        norm = std::sqrt(norm);
        for (unsigned int j = 0; j < dimension; j++)
        {
            rndpoint[j] /= norm;
        }
        rndprojections[i] = rndpoint;
    }
    buckets.resize(L);
    buffer.reserve(maxNumOfCFs + 1);
    projection_buffer.reserve(maxNumOfCFs + 1);
}

template<typename T> int Bico<T>::calcBucketNumber(int rnd, double val)
{
    return (int) floor((val - borders[rnd].first) / bucket_radius[rnd]);
}

template<typename T> void Bico<T>::initializeNN()
{
    double maxBuckets = 10000;
    double Size = 0;
    for (unsigned int i = 0; i < L; i++)
    {
        // Compute new bucket size
        if (buckets[i].size() == 1)
        {
            Size = 1;
        }
        else
        {
            bucket_radius[i] = (long long int) ceil(sqrt(getR(1)));
            Size = (int) ceil((borders[i].second - borders[i].first) / (double) bucket_radius[i]);
            if(Size < 0 || Size > maxBuckets)
            {
                bucket_radius[i] = (borders[i].second - borders[i].first) / maxBuckets;
                Size = (int) ceil((borders[i].second - borders[i].first) / (double) bucket_radius[i]);
            }
        }
        for (unsigned int l = 0; l < buckets[i].size(); l++) buckets[i][l].clear();
        // Create new buckets
        buckets[i].clear();
        buckets[i].resize((int) ceil(Size));
    }
}

template<typename T> void Bico<T>::allocateBucket(int bucket, bool left)
{
    if (left)
    {
        // Push front bucket
        borders[bucket].first = 2 * borders[bucket].first - borders[bucket].second;
        std::vector < std::vector<typename BicoNode::FeatureList::iterator >> a(2 * buckets[bucket].size());
        for (unsigned int i = 0; i < buckets[bucket].size(); i++)
        {
            a[buckets[bucket].size() + i] = buckets[bucket][i];
        }
        for (unsigned int l = 0; l < buckets[bucket].size(); l++) buckets[bucket][l].clear();
        buckets[bucket].clear();
        buckets[bucket] = a;
    }
    else
    {
        // Push back bucket
        borders[bucket].second = 2 * borders[bucket].second - borders[bucket].first;
        std::vector < std::vector<typename BicoNode::FeatureList::iterator >> a(2 * buckets[bucket].size());
        for (unsigned int i = 0; i < buckets[bucket].size(); i++)
        {
            a[i] = buckets[bucket][i];
        }
        for (unsigned int l = 0; l < buckets[bucket].size(); l++) buckets[bucket][l].clear();
        buckets[bucket].clear();
        buckets[bucket] = a;
    }
}

template<typename T> double Bico<T>::project(T point, int i)
{
    double ip = 0.0;
    for (unsigned int j = 0; j < dimension; j++)
    {
        ip += point[j]*(rndprojections[i][j]);
    }
    return ip;
}

template<typename T> ProxySolution<T>* Bico<T>::compute()
{
    ProxySolution<T>* result = new ProxySolution<T>();
    if(bufferPhase)
    {
        result->proxysets.push_back(buffer);
    }
    else
    {
        result->proxysets.push_back(std::vector<T>());
        result->proxysets[0].reserve(curNumOfCFs);
        computeTraverse(root, result);
    }
    return result;
}

template<typename T> void Bico<T>::computeTraverse(BicoNode* node, ProxySolution<T>* solution)
{
    for (auto it = node->begin(); it != node->end(); ++it)
    {
        T point(it->first.cog());
        weightModifier->setWeight(point, it->first.number);
        solution->proxysets[0].push_back(point);
        computeTraverse(it->second, solution);
    }
}

template<typename T> Bico<T>& Bico<T>::operator<<(T const & element)
{
    if (bufferPhase)
    {
        // Update bucket configuration
        for (unsigned int i = 0; i < L; i++)
        {
            double val = std::abs(project(element, i));
            if (val > maxVal[i] || maxVal[i] == -1)
            {
                maxVal[i] = val;
            }
        }

        // Insert point into buffer
        buffer.push_back(element);
        ++pairwise_different;

        // Project point for find nearest neighbor
        double projected = project(element, 0);
        projection_buffer.push_back(std::pair<double, T const *>(projected, &buffer.back()));

        // Enough pairwise different elements to estimate optimal cost?
        if (pairwise_different >= maxNumOfCFs + 1)
        {
            // Sort projection values and determine smallest distance on projection line
            std::sort(projection_buffer.begin(), projection_buffer.end(), comparePairFirst<>());
            double minProjRealDist = std::numeric_limits<double>::infinity();
            for(unsigned int i = 0; i < pairwise_different-2; ++i)
            {
                double tmpDist = projection_buffer[i+1].first - projection_buffer[i].first;
                if(tmpDist < minProjRealDist)
                {
                    double tmpMinProjRealDist = measure->dissimilarity(*projection_buffer[i].second, *projection_buffer[i+1].second);
                    if(tmpMinProjRealDist > 0)
                    {
                        minProjRealDist = tmpMinProjRealDist;
                    }
                }
            }

            // Compute approximate minimum pairwise distance
            int lowerIndex = 0;
            int upperIndex = 0;
            double lowerEnd = projection_buffer[0].first;
            double upperEnd = lowerEnd + minProjRealDist;
            double minDist = minProjRealDist;
            for(unsigned int i = 0; i < pairwise_different-1; ++i)
            {
                if(projection_buffer[i].first >= upperEnd)
                {
                    upperIndex = i;

                    for(int j = lowerIndex; j < upperIndex; ++j)
                    {
                        for(int k = j+1; k < upperIndex; ++k)
                        {
                            //std::cout << "Bulk " << i << ": Distance calculation point " << j << "," << k << std::endl;
                            double tmpDist = measure->dissimilarity(*projection_buffer[j].second, *projection_buffer[k].second);
                            if(tmpDist < minDist && tmpDist > 0)
                            {
                                minDist = tmpDist;
                            }
                        }
                    }

                    lowerIndex = i;
                    lowerEnd = projection_buffer[i].first;
                    upperEnd = lowerEnd + minProjRealDist;
                }
            }

            // Construct buckets
            optEst = 16.0 * minDist;
            //std::cout << "minDist = " << minDist << std::endl;
            //std::cout << "optEst  = " << minDist << std::endl;
            long long int radius = (long long int) ceil(sqrt(getR(1)));
            borders.resize(L);
            for (unsigned int i = 0; i < L; i++)
            {
                borders[i].first = -maxVal[i];
                borders[i].second = maxVal[i];
                bucket_radius[i] = radius;
            }
            initializeNN();

            // Insert buffered elements into tree
            for (auto it = buffer.begin(); it != buffer.end(); ++it)
                insert(root, 1, *it);
            buffer.resize(0);
            bufferPhase = false;
        }
    }
    else
        insert(root, 1, element);
    return *this;
}

template<typename T> void Bico<T>::insertIntoNN(typename BicoNode::FeatureList::iterator iteratorElement)
{
    for (unsigned int i = 0; i < L; i++)
    {
        double val = project(iteratorElement->first.representative, i);
        int bucket_number = calcBucketNumber(i, val);

        if (bucket_number < 0)
        {
            while (bucket_number < 0)
            {
                allocateBucket(i, true);
                bucket_number = calcBucketNumber(i, val);
            }
        }
        else if (bucket_number > (int)buckets[i].size() - 1)
        {
            while (bucket_number > (int)buckets[i].size() - 1)
            {
                allocateBucket(i, false);
                bucket_number = calcBucketNumber(i, val);
            }
        }
        buckets[i][bucket_number].push_back(iteratorElement);
    }
}

template<typename T> void Bico<T>::insert(BicoNode* node, int level, T const & element)
{

    //if (optEst < 0)
    //    throw (InvalidRuntimeConfigurationException(0, "Estimated optimal cost invalid"));

    // Determine nearest clustering feature in current node
    typename BicoNode::FeatureList::iterator nearest(node->nearest(element, level));


    // Construct new clustering feature if element is too far away from
    // nearest clustering feature or insert element into nearest
    if (node->empty() || nearest == node->end()
            || measure->dissimilarity(nearest->first.representative, element) > getR(level))
    {
        CFREntry<T> feature(1, element, element * element, element);
        typename BicoNode::FeatureList::iterator itele = node->insert(feature);

        if (level == 1)
        {
            insertIntoNN(itele);
        }

        ++curNumOfCFs;
    }
    else
    {
        T center(nearest->first.cog());
        // Insert element into (a copy of) nearest and compute cost
        // for insertion at current level
        CFEntry<T> testFeature(nearest->first);
        testFeature.insert(element);
        double tfCost = testFeature.kMeansCost(center);

        // Insert element either to current level (if cost remains small)
        // or to higher level
        if (tfCost < getT(level))
        {
            nearest->first.insert(element);
        }
        else
        {
            insert(nearest->second, level + 1, element);
        }
    }

    // Rebuild?
    while (curNumOfCFs > maxNumOfCFs)
    {
        rebuild();
    }
}

template<typename T> void Bico<T>::rebuild()
{
    // Rebuild first level
    BicoNode * oldRoot(this->root);
    this->root = new BicoNode(*this);
    rebuildFirstLevel(this->root, oldRoot);
    oldRoot->clear();
    delete oldRoot;

    // Traverse through tree and merge
    rebuildTraversMerge(this->root, 1);
}

template<typename T> void Bico<T>::rebuildFirstLevel(BicoNode* parent, BicoNode* child)
{
    optEst *= 2.0;
    ++numOfRebuilds;

    initializeNN();

    // The current element it may be spliced around the tree, so nextIt
    // will maintain an iterator pointing at the next element in child
    auto nextIt = child->begin();
    for (auto it = child->begin(); it != child->end(); it = nextIt)
    {
        ++nextIt;
        // Determine clustering feature in parent that is nearest to child
        typename BicoNode::FeatureList::iterator nearest(parent->nearest(it->first.representative, 1));
        if (parent->empty() || nearest == parent->end()
                || measure->dissimilarity(nearest->first.representative, it->first.representative) > getR(1))
        {
            // Move it from child to parent
            child->spliceElementTo(it, parent, parent->end());

            insertIntoNN(it);
        }
        else
        {
            CFEntry<T> testFeature(it->first);
            testFeature += nearest->first;
            if (testFeature.kMeansCost(nearest->first.representative) <= getT(1))
            {
                // Insert it into nearest
                nearest->first += it->first;
                // Make children of it children of nearest
                it->second->spliceAllTo(nearest->second, nearest->second->end());
                // Delete (now) empty child node of it
                it->second->clear();
                delete it->second;
                // Remove it from tree and delete it
                child->erase(it);
                --curNumOfCFs;
            }
            else
            {
                // Make it a child of nearest
                child->spliceElementTo(it, nearest->second, nearest->second->end());
            }
        }
    }
}

template<typename T> void Bico<T>::rebuildTraversMerge(BicoNode* node, int level)
{
    for (auto parentIt = node->begin(); parentIt != node->end(); ++parentIt)
    {
        if (!parentIt->second->empty())
        {
            auto nextIt = parentIt->second->begin();
            for (auto childIt = parentIt->second->begin(); childIt != parentIt->second->end(); childIt = nextIt)
            {
                ++nextIt;

                T center(parentIt->first.cog());
                // Insert element into (a copy of) nearest and compute cost
                // for insertion at current level
                CFEntry<T> testFeature(parentIt->first + childIt->first);
                double tfCost = testFeature.kMeansCost(center);

                // Merge if possible
                if (tfCost < getT(level))
                {
                    parentIt->first += childIt->first;
                    childIt->second->spliceAllTo(parentIt->second, parentIt->second->end());
                    childIt->second->clear();
                    delete childIt->second;
                    parentIt->second->erase(childIt);
                    --curNumOfCFs;
                }
                else
                {
                    rebuildTraversMerge(childIt->second, level + 1);
                }
            }
        }
    }
}

template<typename T> double Bico<T>::getT(int level)
{
    return optEst;
}

template<typename T> double Bico<T>::getR(int level)
{
    return getT(level) / (double(1 << (3 + level)));
}

template<typename T> void Bico<T>::print(std::ostream& os)
{
    os << "digraph G{\n";
    os << "node [shape=record];\n";
    print(os, root);
    os << "}\n";
}

template<typename T> void Bico<T>::print(std::ostream& os, BicoNode* node)
{
    int id = node->id();

    os << "node" << id << "[label=\"";
    int fvalue = 0;
    os << node->id() << "|";
    for (auto it = node->begin(); it != node->end(); ++it)
    {
        if (fvalue > 0)
            os << "|";
        os << "<f" << fvalue << "> " << it->first.number << "," << it->first.representative
                << "\\n" << it->first.LS << "," << it->first.SS;
        fvalue++;
    }
    os << "\"];\n";

    fvalue = 0;
    for (auto it = node->begin(); it != node->end(); ++it)
    {
        print(os, it->second);
        os << "node" << id << ":f" << fvalue << " -> node" << it->second->id() << ";\n";
        fvalue++;
    }
}

}

#endif
