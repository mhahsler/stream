#ifndef SHC_DeltaLogger_hpp
#define SHC_DeltaLogger_hpp

#include <Eigen/Dense>
#include <iostream>
#include <string>
#include <set>
#include <unordered_map>
#include "SHC_Component"
#include "SHC_ComponentConnection"
using namespace Eigen;
using namespace std;

struct DL_Component_SimpleData {
    VectorXd *mean=NULL;
    bool isInverted=false;
    MatrixXd *covariance=NULL;
    long elements=1;
    DL_Component_SimpleData(SHC_Component *c) {
        mean=new VectorXd(*c->getMean());
        isInverted=c->isCovarianceInverted();
        covariance=c->getCovariance();
        //cout << "Covariance:" << *covariance << endl;
        elements=c->getElements();
    }
    ~DL_Component_SimpleData() {
        if(mean) delete mean;
        if(covariance) delete covariance;
    }
};

struct DL_Component {
    string id,parent_id,*redirectedTo=NULL;
    DL_Component_SimpleData *start=NULL,*end=NULL;
    unordered_map<string,long> delta_elements;
    DL_Component_SimpleData *baseline=NULL;
    set<string> neighborhood,trace;
    bool created=false,outlier=true,obsolete=false,blocked=false;
    string source="master";
    DL_Component(string id,string parent_id) {
        this->id=id;
        this->parent_id=parent_id;
    }
    ~DL_Component() {
        if(redirectedTo) delete redirectedTo;
        if(start) delete start;
        if(end) delete end;
        if(baseline) delete baseline;
    }
};

struct DL_ComponentConnection {
    string c1_id,c2_id;
    long points=0;
    DL_ComponentConnection(SHC_ComponentConnection *cc) {
        c1_id=cc->getComponent1()->getId();
        c2_id=cc->getComponent2()->getId();
    }
};

class DeltaLogger {
public:
    unordered_map<string,DL_Component*> c_delta;
    unordered_map<SHC_ComponentConnectionId<string, string>, DL_ComponentConnection*, SHC_ComponentConnection_hash> cc_delta;
    set<string> cr_delta;

    DeltaLogger()=default;
    ~DeltaLogger();
    DL_Component *logComponent(SHC_Component* logged_comp,string source,int init_delta_elements=0);
    void addComponentDeltaElements(SHC_Component* logged_comp,string source,int number=1);
    void finalizeComponent(SHC_Component* logged_comp);
    void logComponentConnection(SHC_ComponentConnection *logged_cc);
    void logComponentRemoval(SHC_Component* removed_comp);
    void print(ostream &o_str,string source);
};

#endif /* SHC_DeltaLogger_hpp */
