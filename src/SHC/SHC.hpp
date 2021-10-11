#ifndef SHC_hpp
#define SHC_hpp

#include <Eigen/Dense>
#include <Eigen/Eigenvalues>
#include <mutex>
#include <string>
#include <set>
#include <thread>
#include <unordered_map>
#include <memory>
#include "SHC_Component.hpp"
#include "SHC_ComponentConnection.hpp"
#include "SHC_DeltaLogger.hpp"
#include "SigmaIndex.hpp"
#include <chrono>
using namespace std;
using namespace Eigen;

class SHC_Exception : public exception {
private:
    const char *msg=NULL;
public:
    const char *what() const throw();
    SHC_Exception(const char *msg);
};

struct ClassificationResult {
    string *component_id=NULL,*cluster_id=NULL;
    bool outlier=false;
    ~ClassificationResult() {
        if(component_id!=NULL) delete component_id;
        if(cluster_id!=NULL) delete cluster_id;
    }
};
struct _int_cr {
    SHC_Component *comp=NULL;
    double min_md=-1;
    vector<pair<SHC_Component*,double>> *neighborhood_map=NULL,*classified_map=NULL;
    _int_cr(SHC_Component *comp,vector<pair<SHC_Component*,double>> *neighborhood_map,
            vector<pair<SHC_Component*,double>> *classified_map,double min_md) {
        this->comp=comp;
        this->neighborhood_map=neighborhood_map;
        this->classified_map=classified_map;
        this->min_md=min_md;
    }
    ~_int_cr() {
        if(neighborhood_map!=NULL) delete neighborhood_map;
        if(classified_map!=NULL) delete classified_map;
    }
    unordered_map<SHC_Component*,double> *getCombinedNeighborhood();
};

enum AgglomerationType {RelaxedAgglomeration,NormalAgglomeration,AggresiveAgglomeration};
enum DriftType {NormalDrift,FastDrift,SlowDrift,NoDrift,UltraFastDrift};

enum SHCEventType {BeforeObsoleteComponentDeleted,UnknownEvent,DriftFrontTrigger,AfterObsoleteComponentDeleted,
                   SlicePositionChange};
struct SHCEvent {
    string *component_id=NULL,*cluster_id=NULL;
    int *slice_pos=NULL;
    SHCEventType eventType=UnknownEvent;
    SHC *shc;
    SHCEvent(SHCEventType eventType,string id) {
        this->eventType=eventType;
        if(eventType==BeforeObsoleteComponentDeleted || eventType==DriftFrontTrigger || eventType==AfterObsoleteComponentDeleted)
            this->component_id=new string(id);
    }
    SHCEvent(SHCEventType eventType,int pos) {
        this->eventType=eventType;
        if(eventType==SlicePositionChange) this->slice_pos=new int(pos);
    }
    SHCEvent(SHCEventType eventType,SHC *shc,string id) {
        this->eventType=eventType;
        if(eventType==DriftFrontTrigger) {
            this->component_id=new string(id);
            this->shc=shc;
        }
    }
    ~SHCEvent() {
        if(component_id!=NULL) delete component_id;
        if(cluster_id!=NULL) delete cluster_id;
        if(slice_pos!=NULL) delete slice_pos;
    }
};

class SHC {
private:
    SigmaIndex<SHC_Component*> *sigma_index=NULL;
    string deltaLoggingSourceName="master";
    bool parallelize=false,performSharedAgglomeration=true,cache=false,decay=true,cloned=false;
    int agglo_count,agglo_counter,decay_pace=10,sha_agglo_theta=1,entry=0;
    long cbNLimit=40,qTime=0,nodeCounter=0,uTime=0,pTime=0,compTime=0;
    double theta=3.0,cbVarianceLimit=10.0;
    float driftRemoveCompSizeRatio=0.1,driftCheckingSizeRatio=1.0,driftMovementMDThetaRatio=1.0,componentFormingMinVVRatio=0.1,
        componentBlockingLimitVVRatio=0.0;
    VectorXd *virtualVariance=NULL;
    vector<VectorXd> _cache;
    unordered_map<string,vector<VectorXd>> _cache2;
    unordered_map<string,SHC_Component *> components;
    SHC_ComponentConnection_Set connections;
    SHC_Containable_Set *containers=NULL;
    static void t1(SHC *shc, SHC_Component *comp, VectorXd *newElement, vector<pair<SHC_Component*,double>> *classified_map, vector<pair<SHC_Component*,double>> *neighborhood_map,
                   vector<pair<SHC_Component*,double>> *obsolete_map, double theta=3.0);
    //static void t2(SHC *shc, SHC_Component *comp_from, SHC_Component *comp_to, vector<pair<string*,double*>> *res_map);
    pair<bool, bool> agglomerate(SHC_Component *c1,SHC_Component *c2,bool add_point=true);
    void joinComponents(SHC_Component *c1,SHC_Component *c2);
    void purgeEmptyContainers();
    pair<set<string>*,set<string> *> getOutliersAndComponents();
    set<SHC_Component *> *getUnrelatedComponents(SHC_Component *comp);
    _int_cr classify_p1(bool classifyOnly,vector<pair<SHC_Component*,double>> *obsolete_map,vector<pair<SHC_Component*, double>> *classified_map,
                        vector<pair<SHC_Component*,double>> *neighborhood_map/*,vector<thread*> *workers*/);
    _int_cr classify(VectorXd *newElement, bool classifyOnly=false, set<SHC_Component *> *excludeComponents=NULL);
    _int_cr classifySigmaIndex(VectorXd *newElement, bool classifyOnly=false, set<SHC_Component*> *excludeComponents=NULL, SHC_Component *starting=NULL);
    _int_cr classify(VectorXd *newElement, SHC_Component *parentComponent, bool classifyOnly=false);
    _int_cr classify(VectorXd *newElement, vector<SHC_Component*> *forComponents, bool classifyOnly=false);
    function<void (SHCEvent*)> eventCallback;
    set<string> decayedOutliers;
    shared_ptr<DeltaLogger> delta=nullptr;
protected:
    mutex m1;
public:
    SHC(double theta,bool parallelize=false,bool performSharedAgglomeration=true,VectorXd *virtualVariance=NULL,
        int agglo_count=100,double cbVarianceLimit=10.0,long cbNLimit=40,float driftRemoveCompSizeRatio=0.1,
        float driftCheckingSizeRatio=1.0,float driftMovementMDThetaRatio=1.0,int decayPace=10,
        float componentFormingMinVVRatio=0.1, float componentBlockingLimitVVRatio=0.0, bool cache=false);
    SHC(int dimensions,AgglomerationType aggloType=NormalAgglomeration,DriftType driftType=NormalDrift,
        int decayPace=10,bool cache=false,bool parallelize=false);
    SHC(SHC *cloneFrom);
    ~SHC();
    shared_ptr<ClassificationResult> process(VectorXd *newElement, bool classifyOnly=false);
    pair<shared_ptr<vector<shared_ptr<ClassificationResult>>>,shared_ptr<DeltaLogger>> process(MatrixXd *elements, bool initNewDeltaLogger=false, bool classifyOnly=false);
    SHC_Component *getComponent(string *comp_id);
    set<string> *getTopContainers(bool clusters=true, bool outliers=true);
    vector<SHC_Component_Details *> *getClassificationDetails(string *container_id,double theta,int dim1,int dim2,bool single=false);
    vector<SHC_Component_Details *> *getClassificationDetails(string *container_id);
    void print(ostream &o_str);
    void pseudoOffline(bool force=false);
    void processObsoleteComponent(SHC_Component *comp);
    double calculateFragmentationIndex();
    SHC_Component *getBiggestComponent();
    long getTotalElements(bool components=true,bool outliers=true);
    unordered_map<string, SHC_Component*> *getComponents();
    vector<SHC_Component*> *getComponents(string *container_id);
    vector<SHC_Component*> *getOutliers();
    void merge(SHC *mergedClassifier, SHC_Component *obsoleteComponent=NULL, unordered_map<string, std::set<string>*> *front_conn_map=NULL);
    SHC *clone();
    void setAgglomerationCount(int agglo_count);
    void setEventCallback(function<void (SHCEvent*)> eventCallback);
    set<string> *traceComponentId(string *id);
    MatrixXd *getCache();
    MatrixXd *getCache(string *id);
    double meanWeightedComponentMDFrom(SHC_Component *fromComponent);
    void switchDecay(bool decay);
    double getTheta();
    VectorXd *getVirtualVariance();
    void cleanOutliers();
    bool recheckOutlier(string *id);
    void setSharedAgglomerationThreshold(int sha_agglo_theta);
    void agglomerateComponents(SHC_Component *c1,SHC_Component *c2);
    pair<long,long> getStatistic();    
    void removeComponent(SHC_Component *c);
    void setComponentFormingVVRatio(float ratio);
    void setComponentBlockingLimitVVRatio(float ratio);
    void removeSmallComponents(int elementsThreshold);
    void useSigmaIndex(int neighborhood_mutiplier=2, bool precision_switch=true);
    void printSigmaIndex(ostream &o_str);
    vector<double> getTimes();
    long getNodeCounter();
    SigmaIndex<SHC_Component*> *getSigmaIndex();
    void consumeDeltaLog(shared_ptr<DeltaLogger> delta_log,string *delta_log_src=NULL,shared_ptr<DeltaLogger> amending_log=nullptr,
                         bool dropCosumationActivity=false,ostream *o_str=NULL);
    void incDeltaElementsInDeltaLog(SHC_Component *comp,int number=1);
    void setDeltaLoggingSourceName(string s);
    string getDeltaLoggingSourceName();
    void clearEigenMPSupport();
};

#endif /* SHC_hpp */
