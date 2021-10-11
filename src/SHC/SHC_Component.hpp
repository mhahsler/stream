#ifndef SHC_Component_hpp
#define SHC_Component_hpp

#include <Eigen/Dense>
#include <mutex>
#include <string>
#include <set>
#include <vector>
#include <unordered_map>
#include "SHC_Container.hpp"
#include <memory>
#include "SHC_Decay.hpp"
using namespace Eigen;
using namespace std;

class SHC_Count_Decay;
class SHC_Component_Exception : public exception {
private:
    const char *msg=NULL;
public:
    const char *what() const throw();
    SHC_Component_Exception(const char *msg);
};

const double DEFAULT_VIRTUAL_VARIANCE=0.7;

class SHC_Component;
struct SHC_Component_Details {
    MatrixXd *upper=NULL,*lower=NULL,*single=NULL;
    bool covThreshold=false,nThreshold=false,obsolete=false;
    VectorXd *mean=NULL,*baseline=NULL;
    vector<SHC_Component_Details *> childDetails;
    SHC_Component_Details *baselineDetails=NULL;
    SHC_Component *parent=NULL;
    SHC_Component_Details(SHC_Component *parent, MatrixXd *upper, MatrixXd *lower, bool covThreshold, bool nThreshold, bool obsolete,
                          VectorXd *mean=NULL, VectorXd *baseline=NULL) {
        this->parent=parent;
        this->upper=upper;
        this->lower=lower;
        this->covThreshold=covThreshold;
        this->nThreshold=nThreshold;
        this->obsolete=obsolete;
        this->mean=mean;
        this->baseline=baseline;
    }
    SHC_Component_Details(SHC_Component *parent, MatrixXd *single, bool covThreshold, bool nThreshold, bool obsolete, VectorXd *mean=NULL,
                          VectorXd *baseline=NULL) {
        this->parent=parent;
        this->single=single;
        this->covThreshold=covThreshold;
        this->nThreshold=nThreshold;
        this->obsolete=obsolete;
        this->mean=mean;
        this->baseline=baseline;
    }
    ~SHC_Component_Details() {
        if(upper!=NULL) delete upper;
        if(lower!=NULL) delete lower;
        if(single!=NULL) delete single;
        if(mean!=NULL) delete mean;
        if(baseline!=NULL) delete baseline;
        for(SHC_Component_Details *det:childDetails) delete det;
        if(baselineDetails) delete baselineDetails;
    }
};

class SHC;
struct SHCEvent;
class SHC_Component : public SHC_Containable {
private:
    string source_node="master";
    SHC_Component *redirect_to=NULL,*baseline=NULL;
    unordered_map<string, SHC_Component*> redirected_from;
    SHC *front_resolver=NULL;
    unordered_map<string, std::set<string>*> *front_connection_mapping=NULL;
    std::set<SHC_Component*> neighborhood;
    VectorXd *mean=NULL;
    int decay_pace=10,agglo_out_blocker=0;
    long elements=0,dim=0,cbNLimit=40;
    double cbVarianceLimit=10.0;
    float driftCheckingSizeRatio=1.0,driftMovementMDThetaRatio=0.5,componentFormingMinVVRatio=0.1,componentBlockingLimitVVRatio=0.0;
    bool inverted=false,outlier=true,obsolete=false,blocked=false;
    MatrixXd *cov=NULL,*vv=NULL;
    double vv_value=DEFAULT_VIRTUAL_VARIANCE;
    bool isCovInvertible();
    MatrixXd *chooseUniOrMulti();
    VectorXd *getEllipticalCoordinates(double angle,double R,int dim1,int dim2);
    pair<double, VectorXd*> *getEllipticalThresholdRadius(double theta,double angle,double initR,int dim1,int dim2);
    function<void (SHCEvent*)> eventCallback;
    SHC_Count_Decay *decay_handler=NULL;
    void cleanRedirection();
protected:
public:
    SHC_Component(SHC_Containable_Set *set,VectorXd *newElement,VectorXd *virtualVariance=NULL,double cbVarianceLimit=10.0,long cbNLimit=40,
                  float driftCheckingSizeRatio=1.0,float driftMovementMDThetaRatio=0.5,int decayPace=10,float componentFormingMinVVRatio=0.1,
                  float componentBlockingLimitVVRatio=0.0);
    SHC_Component(SHC_Containable_Set *set,MatrixXd *initDataSet,VectorXd *virtualVariance=NULL,double cbVarianceLimit=10.0,long cbNLimit=40,
                  float driftCheckingSizeRatio=1.0,float driftMovementMDThetaRatio=0.5,int decayPace=10,float componentFormingMinVVRatio=0.1,
                  float componentBlockingLimitVVRatio=0.0);
    SHC_Component(SHC_Containable_Set *set,VectorXd *mean,MatrixXd *covariance,bool inverted,long elements,
                  VectorXd *virtualVariance=NULL);
    SHC_Component(SHC_Containable_Set *set,SHC_Component *cloneFrom);
    ~SHC_Component();
    bool addNewElement(VectorXd *newElement,SHC *parent_resolver,vector<pair<SHC_Component*,double>> *classified_map=NULL);
    double mahalanobisDistance(VectorXd *newElement);
    double euclideanDistance(SHC_Component *comp);
    double connectionMeasure(SHC_Component *comp,double theta);
    void print(ostream &o_str);
    VectorXd *getMean();
    void setMean(VectorXd *new_mean);
    MatrixXd *getCovariance();
    void setCovariance(MatrixXd *new_cov,bool new_inverted=false);
    long getElements();
    void setElements(long new_elements);
    bool isOutlier();
    bool isCovarianceInverted();
    void setObsolete(bool obsolete);
    bool isObsolete();
    bool isBlocked();
    void setBlocked(bool blocked);
    bool hasBaseline();
    SHC_Component *getBaseline();
    unordered_map<string, SHC_Containable *> fetchChildComponents();
    SHC_Component_Details *calculateDetails(double theta,int dim1,int dim2,double cbVarianceLimit,double cbNLimit,bool single=false);
    SHC_Component_Details *calculateDetails(double theta);
    void redirectComponent(SHC_Component *to_c);
    bool isRedirected();
    SHC_Component *getRedirectedComponent(),*getFinalRedirectedComponent();
    void addFromRedirectedComponent(SHC_Component *from_c);
    void removeFromRedirectedComponent(SHC_Component *from_c);
    bool operator==(const SHC_Component &c) const;
    SHC_Component *clone(SHC_Containable_Set *set_forClone=NULL, bool retainId=false);
    void resetBaseline();
    void setBaselineLimits(double cbVarianceLimit,long cbNLimit);
    SHC *getFrontResolver();
    void setEventCallback(function<void (SHCEvent*)> eventCallback);
    void setDriftOptions(float sizeRatio=1.0,float movementRatio=0.5);
    bool decayPing();
    void switchDecay(bool decay,int decayPace);
    SHC_Count_Decay *getDecayHandler();
    void setId(string id);
    long getDimensions();
    void addNeighbor(SHC_Component *neighbor,SHC *parent=NULL,bool measure=false);
    void removeNeighbor(SHC_Component *neighbor);
    void findNeighbors(SHC *parent);
    void agglomerateNeighborhood(SHC *parent);
    void removeFromNeighborhood(SHC *parent);
    std::set<SHC_Component*> *getNeighborhood();
    void setSourceNode(string source_node);
    string getSourceNode();
};

#endif /* SHC_Component_hpp */
