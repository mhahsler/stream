#ifndef SigmaIndex_hpp
#define SigmaIndex_hpp

#include <string>
#include <set>
#include <vector>
#include <unordered_map>
#include <memory>
#include <iostream>
#include "SHC_Component"
#include <Eigen/Dense>
#include <cmath>
#include "SigmaIndexProxy"
using namespace std;
using namespace Eigen;

const string ROOT="root";

class SHC_Component;

struct _SigmaIndexQueryResults {
    bool visited=false;
    double md=-1;
    void reset() {
        visited=false;
        md=-1;
    }
    int getClass(double theta,double ntheta) {
        if(md>=0 && md<=theta) return 1;
        if(md>theta && md<=ntheta) return 2;
        return 0;
    }
    _SigmaIndexQueryResults clone() {
        _SigmaIndexQueryResults res;
        res.visited=this->visited;
        res.md=this->md;
        return res;
    }
};

/**
 Sigma-Index node class. Template T represents a population class used in the Sigma-Index node.
 */
template<class T>
class SigmaIndexNode {
private:
    string bckp_id; // node identifier... usually this is taken from the population identifier
protected:
    T pop_ptr=NULL; // population object pointer
    unordered_map<string,SigmaIndexNode<T>*> parents,children; // maps to parent and child nodes... the key of the map is node identifier
public:
    SigmaIndexNode();
    SigmaIndexNode(T population);
    ~SigmaIndexNode();
    string getId();
    T getPopulation();
    void addChild(SigmaIndexNode<T> *child);
    void removeChild(SigmaIndexNode<T> *child);
    bool isDirectChild(SigmaIndexNode<T> *node);
    bool isDirectParent(SigmaIndexNode<T> *node);
    bool underTheRoot();
    unordered_map<string, SigmaIndexNode<T>*> *getChildren(), *getParents();
    vector<SigmaIndexNode<T>*> *getParentsVector();
    vector<SigmaIndexNode<T>*> *getChildrenVector();
    void removeRootParent(SigmaIndexNode<T> *root_node);
    void moveTo(SigmaIndexNode<T> *pn);
    vector<SigmaIndexNode<T>*> *getSortedChildren();
    vector<SigmaIndexNode<T>*> *getSortedParents();
    bool operator==(const SigmaIndexNode<T> &c) const;
    bool operator!=(const SigmaIndexNode<T> &c) const;
    _SigmaIndexQueryResults results;
    SigmaIndexNode<T> *clone();
};

/**
 Sigma-Index query results structure. Template V represents the population class.
 */
template<class V>
struct SigmaIndexQueryResults {
    // classified = populations and mahalanobis distance under theta
    // neighborhood = populations and related mahalanobis distances between theta and ntheta (neighborhood threshold)
    vector<pair<V,double>> *classified=NULL, *neighborhood=NULL;
    SigmaIndexQueryResults() {
        classified=new vector<pair<V,double>>();
        neighborhood=new vector<pair<V,double>>();
    }
};
/**
 Sigma-Index statistics structure.
 */
struct SigmaIndexStatistics {
    // totalCount - total populations that needed Mahalanobis distance calculations
    // tpCount - total populations within ntheta
    // tsCound - total populations outside ntheta - those that were "missed" in the previous queries
    // totalSequential - total number of populations for previous queries
    long totalCount=0,tpCount=0,tsCount=0,totalSequential=0,qTime=0;
    // R - computation cost reduction
    double R=0.0;
    SigmaIndexStatistics(long totalCount,long tpCount,long tsCount,long totalSequential,long qTime) {
        this->totalCount=totalCount;
        this->tpCount=tpCount;
        this->tsCount=tsCount;
        this->totalSequential=totalSequential;
        this->R=totalSequential>0 && totalCount<=totalSequential ? ((double)totalSequential-(double)totalCount)/(double)totalSequential : 0;
        this->qTime=qTime;
    }
};

/**
 Sigma-Index exception
 */
class SigmaIndex_Exception : public exception {
private:
    const char *msg=NULL;
public:
    const char *what() const throw();
    SigmaIndex_Exception(const char *msg);
};

/**
 Sigma-Index class. Template U represents population class.
 */
template<class U>
class SigmaIndex {
private:
    int c1=0,c2=0; // used as a counter in queries
    bool switch1=false; // precision switch 1
    long nodeCounter=0,tpCount=0,tsCount=0,totalSequential=0,qTime=0; // statistics counters
    double theta=3.0,ntheta=6.0; // statistical thresholds
    bool cached=false; // a flag that indicates that Sigma-Index has cached the last queried data point
    VectorXd *cachedData=NULL; // cached data point
    void connect(SigmaIndexNode<U> *pn, SigmaIndexNode<U> *cn);
    void disconnect(SigmaIndexNode<U> *pn, SigmaIndexNode<U> *cn);
    void move(SigmaIndexNode<U> *pn1, SigmaIndexNode<U> *cn, SigmaIndexNode<U> *pn2);
    void _query(VectorXd *data,SigmaIndexNode<U> *node,SigmaIndexNode<U> *results[]);
    void make_connections(SigmaIndexNode<U> *c,SigmaIndexNode<U> *n,bool force=false);
    void _resetCache();
    void _completeUpdate(SigmaIndexNode<U> *class_node);
protected:
    unordered_map<string, SigmaIndexNode<U> *> nodes; // the main map which holds the Sigma-Index nodes
public:
    SigmaIndex(double theta, double neighborhood_theta, bool precision_switch=true);
    ~SigmaIndex();
    void update(U classified,unordered_map<U,double> *neighborhood);
    SigmaIndexQueryResults<U> *query(VectorXd *data,set<U> *exclude=NULL,U starting=NULL,
                                     bool resetCache=true);
    void create(U comp,bool resetCache=false);
    void remove(U comp);
    void print(string node_id,ostream &ostr,set<string> *visited=NULL);
    int getNeighborhoodMultiplier();
    SigmaIndex *clone();
    void resetStatistics();
    SigmaIndexStatistics *getStatistics();
    unordered_map<int, long> *calculateHistogram();
    void completeUpdate(U classified);
    SigmaIndexNode<U> *getRoot();
};

#include "SigmaIndex_Inline.cpp"


#endif /* SigmaIndex_hpp */
