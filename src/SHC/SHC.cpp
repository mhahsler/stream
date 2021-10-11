#include <thread>
#include <unordered_map>
#include "SHC.hpp"
#include "SHC_Component.hpp"
#include "SHC_Container.hpp"
#include <string>
#include <iostream>
#include <set>
#include <numeric>
#include <memory>

using namespace std;

SHC_Exception::SHC_Exception(const char *msg) {
    this->msg=msg;
}
const char *SHC_Exception::what() const throw() {
    return this->msg;
}

SHC::SHC(double theta, bool parallelize, bool performSharedAgglomeration, VectorXd *virtualVariance, int agglo_count, double cbVarianceLimit,
         long cbNLimit, float driftRemoveCompSizeRatio, float driftCheckingSizeRatio, float driftMovementMDThetaRatio, int decayPace,
         float componentFormingMinVVRatio, float componentBlockingLimitVVRatio, bool cache) {
    this->parallelize=parallelize;
    this->performSharedAgglomeration=performSharedAgglomeration;
    this->decay_pace=decayPace;
    this->theta=theta;
    this->virtualVariance=new VectorXd(*virtualVariance);
    this->agglo_count=agglo_count;
    this->agglo_counter=agglo_count;
    this->cbVarianceLimit=cbVarianceLimit;
    this->cbNLimit=cbNLimit;
    this->containers=new SHC_Containable_Set();
    this->driftRemoveCompSizeRatio=driftRemoveCompSizeRatio;
    this->driftCheckingSizeRatio=driftCheckingSizeRatio;
    this->driftMovementMDThetaRatio=driftMovementMDThetaRatio;
    this->componentFormingMinVVRatio=componentFormingMinVVRatio;
    this->componentBlockingLimitVVRatio=componentBlockingLimitVVRatio;
    this->cache=cache;
}

SHC::SHC(int dimensions,AgglomerationType aggloType,DriftType driftType,int decayPace,bool cache,bool parallelize) {
    if(dimensions<1) throw SHC_Exception("Behavioral SHC must have supplied dimensions>0");
    this->parallelize=parallelize;
    this->performSharedAgglomeration=true;
    this->decay_pace=decayPace;
    this->componentFormingMinVVRatio=0.2;
    this->componentBlockingLimitVVRatio=0.0;
    this->cache=cache;
    switch (aggloType) {
        case RelaxedAgglomeration:
            this->theta=2.9;
            this->virtualVariance=new VectorXd(0.8*VectorXd::Ones(dimensions));
            this->agglo_count=50;
            this->driftRemoveCompSizeRatio=0.3;
            break;
        case AggresiveAgglomeration:
            this->theta=3.5;
            this->virtualVariance=new VectorXd(1.2*VectorXd::Ones(dimensions));
            this->agglo_count=1;
            this->driftRemoveCompSizeRatio=0.05;
            break;
        default:
            this->theta=3.2;
            this->virtualVariance=new VectorXd(1.0*VectorXd::Ones(dimensions));
            this->agglo_count=25;
            this->driftRemoveCompSizeRatio=0.1;
            break;
    }
    switch (driftType) {
        case FastDrift:
            this->cbNLimit=40;
            this->cbVarianceLimit=6;
            this->driftCheckingSizeRatio=1.0;
            this->driftMovementMDThetaRatio=0.6;
            break;
        case SlowDrift:
            this->cbNLimit=120;
            this->cbVarianceLimit=9;
            this->driftCheckingSizeRatio=2.0;
            this->driftMovementMDThetaRatio=1.0;
            break;
        case NoDrift:
            this->cbNLimit=0;
            this->cbVarianceLimit=0;
            break;
        case UltraFastDrift:
            this->cbNLimit=20;
            this->cbVarianceLimit=6;
            this->driftCheckingSizeRatio=0.5;
            this->driftMovementMDThetaRatio=0.5;
            this->driftRemoveCompSizeRatio=0.05;
            this->agglo_count=1;
            break;
        default:
            this->cbNLimit=80;
            this->cbVarianceLimit=8;
            this->driftCheckingSizeRatio=1.3;
            this->driftMovementMDThetaRatio=0.8;
            break;
    }
    this->containers=new SHC_Containable_Set();
    this->agglo_counter=this->agglo_count;
}

void SHC::t1(SHC *shc, SHC_Component *comp, VectorXd *newElement, vector<pair<SHC_Component*,double>> *classified_map, vector<pair<SHC_Component*,double>> *neighborhood_map,
             vector<pair<SHC_Component*,double>> *obsolete_map, double theta) {
    
    std::chrono::time_point<std::chrono::system_clock> qst=std::chrono::system_clock::now();
    double md=comp->mahalanobisDistance(newElement);
    std::chrono::time_point<std::chrono::system_clock> qet=std::chrono::system_clock::now();
    shc->qTime+=std::chrono::duration_cast<std::chrono::microseconds>(qet-qst).count();
    shc->nodeCounter++;
    shc->m1.lock();
    if(comp->isObsolete() && md<=theta) obsolete_map->push_back(pair<SHC_Component*,double>(comp,md));
    else if(!comp->isObsolete() && md<=theta) classified_map->push_back(pair<SHC_Component*,double>(comp,md));
    else if(!comp->isObsolete() && md>theta && md<=(3*theta)) neighborhood_map->push_back(pair<SHC_Component*,double>(comp,md));
    shc->m1.unlock();
}

double SHC::getTheta() {
    return theta;
}

VectorXd *SHC::getVirtualVariance() {
    return virtualVariance;
}

_int_cr SHC::classify_p1(bool classifyOnly,vector<pair<SHC_Component*,double>> *obsolete_map,vector<pair<SHC_Component*,double>> *classified_map,
                         vector<pair<SHC_Component*,double>> *neighborhood_map/*,vector<thread *> *workers*/) {
    /*if(parallelize && workers!=NULL)
        for(thread *t:*workers)
            t->join();
    for(thread *t:*workers) delete t;
    delete workers;*/
    sort(classified_map->begin(),classified_map->end(),[=](pair<SHC_Component*,double>& a,pair<SHC_Component*,double>& b) {
        return a.second<b.second;
    });
    sort(neighborhood_map->begin(),neighborhood_map->end(),[=](pair<SHC_Component*,double>& a,pair<SHC_Component*,double>& b) {
        return a.second<b.second;
    });
    if(!classifyOnly && classified_map->size()>0) {
        int offset=1;
        while((classified_map->begin()+offset)!=classified_map->end()) {
            pair<bool,bool> removed=agglomerate((classified_map->begin())->first,(classified_map->begin()+offset)->first);
            if(removed.second) {
                classified_map->erase(classified_map->begin()+offset);
                offset=1;
            } else if(removed.first) {
                classified_map->erase(classified_map->begin());
                offset=1;
            } else offset++;
        }
    }
//    string *agglo_comp=NULL,*min_comp=NULL;
/*    if(!classifyOnly) {
        if(res_map->size()>1) {
            pair<bool, bool> removed=agglomerate(res_map->at(0).first,res_map->at(1).first);
            if(removed.second) {
                res_map->erase(res_map->begin()+1);
            } else if(removed.first) {
                res_map->erase(res_map->begin());
            } else agglo_comp=new string(res_map->at(1).first);
        }
        if(res_map->size()>0) for(auto obs_comp_id:*obs_map) agglomerate(res_map->at(0).first, obs_comp_id.first);
    }*/
    // we changed it slightly
    for(auto c_comp_ip:*classified_map)
        for(auto obs_comp_id:*obsolete_map)
            agglomerate(obs_comp_id.first,c_comp_ip.first);
    SHC_Component *min_comp=NULL;
    double min_md=-1;
    for(auto c_comp_ip:*classified_map)
        if(min_comp==NULL && !c_comp_ip.first->isBlocked()) {
            min_comp=c_comp_ip.first;
            min_md=c_comp_ip.second;
            break;
        }
    if(min_comp==NULL && classified_map->size()>0) {
        min_comp=classified_map->at(0).first;
        min_md=classified_map->at(0).second;
    }
    delete obsolete_map;
    return _int_cr{min_comp,neighborhood_map,classified_map,min_md};
}

_int_cr SHC::classify(Eigen::VectorXd *newElement, bool classifyOnly, set<SHC_Component*> *excludeComponents) {
    vector<pair<SHC_Component*,double>> *classified_map=new vector<pair<SHC_Component*,double>>(),
                                        *obsolete_map=new vector<pair<SHC_Component*,double>>(),
                                        *neighborhood_map=new vector<pair<SHC_Component*,double>>();
    
    for(unordered_map<string,SHC_Component*>::iterator it=components.begin();it!=components.end();it++) {
        if(excludeComponents==NULL || excludeComponents->find(it->second)==excludeComponents->end()) {
            SHC::t1(this,it->second,newElement,classified_map,neighborhood_map,obsolete_map,theta);
        }
    }
    
    return classify_p1(classifyOnly,obsolete_map,classified_map,neighborhood_map/*,workers*/);
}

_int_cr SHC::classifySigmaIndex(Eigen::VectorXd *newElement, bool classifyOnly, set<SHC_Component*> *excludeComponents, SHC_Component *starting) {
    vector<pair<SHC_Component*,double>> *classified_map1=new vector<pair<SHC_Component*,double>>(),
                                        *obsolete_map=new vector<pair<SHC_Component*,double>>(),
                                        *neighborhood_map=NULL;
    if(sigma_index!=NULL) {
        SigmaIndexQueryResults<SHC_Component*> *sigres=sigma_index->query(newElement,excludeComponents,starting);
        
        neighborhood_map=sigres->neighborhood;
        for(pair<SHC_Component*,double> it:*sigres->classified)
            if(it.first->isObsolete()) obsolete_map->push_back(it);
            else classified_map1->push_back(it);
        if(!classifyOnly && classified_map1->size()>0) {
            int offset=1;
            while((classified_map1->begin()+offset)!=classified_map1->end()) {
                pair<bool,bool> removed=agglomerate((classified_map1->begin())->first,(classified_map1->begin()+offset)->first);
                if(removed.second) {
                    classified_map1->erase(classified_map1->begin()+offset);
                    offset=1;
                } else if(removed.first) {
                    classified_map1->erase(classified_map1->begin());
                    offset=1;
                } else offset++;
            }
        }
        for(auto c_comp_ip:*classified_map1)
            for(auto obs_comp_id:*obsolete_map)
                agglomerate(obs_comp_id.first,c_comp_ip.first);
        SHC_Component *min_comp=NULL;
        double min_md=-1;
        for(auto c_comp_ip:*classified_map1)
            if(min_comp==NULL && !c_comp_ip.first->isBlocked()) {
                min_comp=c_comp_ip.first;
                min_md=c_comp_ip.second;
                break;
            }
        if(min_comp==NULL && classified_map1->size()>0) {
            min_comp=classified_map1->at(0).first;
            min_md=classified_map1->at(0).second;
        }
        delete obsolete_map;delete sigres->classified;delete sigres;
        return _int_cr{min_comp,neighborhood_map,classified_map1,min_md};
    } else throw SHC_Exception("Sigma index is not constructed, yet SHC want to use it :-S");
}

_int_cr SHC::classify(Eigen::VectorXd *newElement, SHC_Component *parentComponent, bool classifyOnly) {
    vector<pair<SHC_Component*,double>> *classified_map=new vector<pair<SHC_Component*,double>>(),
                                        *obsolete_map=new vector<pair<SHC_Component*,double>>(),
                                        *neighborhood_map=new vector<pair<SHC_Component*,double>>();
    
    unordered_map<string,SHC_Containable *> ccomps=parentComponent->fetchChildComponents();
    for(unordered_map<string,SHC_Containable *>::iterator it=ccomps.begin();it!=ccomps.end();it++) {
        if(typeid(it->second)==typeid(SHC_Component)) {
            SHC_Component *comp=static_cast<SHC_Component *>(it->second);
            SHC::t1(this,comp,newElement,classified_map,neighborhood_map,obsolete_map,theta);
        }
    }
    
    return classify_p1(classifyOnly,obsolete_map,classified_map,neighborhood_map/*,workers*/);
}

_int_cr SHC::classify(Eigen::VectorXd *newElement, vector<SHC_Component*> *forComponents, bool classifyOnly) {
    vector<pair<SHC_Component*,double>> *classified_map=new vector<pair<SHC_Component*,double>>(),
                                        *obsolete_map=new vector<pair<SHC_Component*,double>>(),
                                        *neighborhood_map=new vector<pair<SHC_Component*,double>>();
    
    for(SHC_Component *comp:*forComponents) {
        SHC::t1(this,comp,newElement,classified_map,neighborhood_map,obsolete_map,theta);
    }
    
    return classify_p1(classifyOnly,obsolete_map,classified_map,neighborhood_map/*,workers*/);
}

shared_ptr<ClassificationResult> SHC::process(VectorXd *newElement, bool classifyOnly) {
    // Decay procedure... we remove at the beginning to reduce query
    vector<string> decay_rem;
    for(auto comp_it:components) {
        if(!comp_it.second->isObsolete() && comp_it.second->decayPing())
            decay_rem.push_back(comp_it.second->getId());
    }
    for(string rcompId:decay_rem) {
        if(components.find(rcompId)!=components.end()) {
            SHC_Component *rcomp=components[rcompId];
            if(rcomp->isOutlier()) {
                decayedOutliers.insert(rcomp->getId());
                removeComponent(rcomp);
            } else processObsoleteComponent(rcomp);
        }
    }
    // Query
    _int_cr class_res=sigma_index==NULL ? classify(newElement, classifyOnly) : classifySigmaIndex(newElement, classifyOnly);
    
    shared_ptr<ClassificationResult> res=make_shared<ClassificationResult>();
    SHC_Component *comp=NULL;
    if(class_res.comp) {
        comp=class_res.comp;
        bool added=false;
        if(!classifyOnly) {
            added=comp->addNewElement(newElement,this,class_res.classified_map);
            if(sigma_index) {
                std::chrono::time_point<std::chrono::system_clock> ust=std::chrono::system_clock::now();
                sigma_index->update(comp->getFinalRedirectedComponent(),class_res.getCombinedNeighborhood());
                std::chrono::time_point<std::chrono::system_clock> uet=std::chrono::system_clock::now();
                uTime+=std::chrono::duration_cast<std::chrono::microseconds>(uet-ust).count();
            }
        }
        res->component_id=new string(comp->getId());
        res->cluster_id=comp->getParent()!=NULL ? new string(comp->getParent()->getId()) : NULL;
    } else {
        if(!classifyOnly) {
            comp=new SHC_Component(containers,newElement,virtualVariance,cbVarianceLimit,cbNLimit,driftCheckingSizeRatio,
                                   driftMovementMDThetaRatio,decay_pace,componentFormingMinVVRatio,componentBlockingLimitVVRatio);
            comp->switchDecay(decay,decay_pace);
            comp->setSourceNode(deltaLoggingSourceName);
            if(eventCallback) comp->setEventCallback(eventCallback);
            SHC_Containable *cluster=new SHC_Containable(containers, comp);
            components[comp->getId()]=comp;
            if(delta) delta->addComponentDeltaElements(comp,deltaLoggingSourceName);
            if(sigma_index) {
                std::chrono::time_point<std::chrono::system_clock> ust=std::chrono::system_clock::now();
                sigma_index->create(comp);
                std::chrono::time_point<std::chrono::system_clock> uet=std::chrono::system_clock::now();
                uTime+=std::chrono::duration_cast<std::chrono::microseconds>(uet-ust).count();
            }
            res->component_id=new string(comp->getId());
            res->cluster_id=new string(cluster->getId());
            res->outlier=true;
        } else res->outlier=true;
    }
    if(!classifyOnly) {
        for(pair<SHC_Component*,double> it:*class_res.neighborhood_map)
            comp->addNeighbor(it.first,this,true);
        comp->agglomerateNeighborhood(this);
        pseudoOffline();
        processObsoleteComponent(comp);
        if(cache) {
            _cache.push_back(*newElement);
            for(pair<SHC_Component*,double> it:*class_res.classified_map) {
                if(_cache2.find(it.first->getId())==_cache2.end()) _cache2[it.first->getId()]=vector<VectorXd>();
                _cache2[it.first->getId()].push_back(*newElement);
            }
        }
    }
    return res;
}

pair<shared_ptr<vector<shared_ptr<ClassificationResult>>>,shared_ptr<DeltaLogger>> SHC::process(MatrixXd *elements, bool initNewDeltaLogger, bool classifyOnly) {
    if(parallelize) {
        if(!delta || (delta && initNewDeltaLogger)) {
            if(delta) delta.reset();
            delta=make_shared<DeltaLogger>();
        }
    }
    qTime=0;uTime=0;compTime=0;
    std::chrono::time_point<std::chrono::system_clock> pst=std::chrono::system_clock::now();
    if(sigma_index) sigma_index->resetStatistics();
    else this->nodeCounter=0;
    shared_ptr<vector<shared_ptr<ClassificationResult>>> res=make_shared<vector<shared_ptr<ClassificationResult>>>();
    if(elements!=NULL) {
        for(int i(0);i<elements->rows();i++) {
            if(eventCallback) eventCallback(new SHCEvent(SlicePositionChange,i));
            VectorXd row=elements->row(i);
            shared_ptr<ClassificationResult> cr=process(&row,classifyOnly);
            res->push_back(cr);
        }
    }
    std::chrono::time_point<std::chrono::system_clock> pet=std::chrono::system_clock::now();
    pTime=std::chrono::duration_cast<std::chrono::microseconds>(pet-pst).count();
    if(delta)
        for(pair<string,SHC_Component*> it:components) delta->finalizeComponent(it.second);
    if(sigma_index) {
        SigmaIndexStatistics *st=sigma_index->getStatistics();
        qTime=st->qTime;
        delete st;
    }
    
    return make_pair(res,delta);
}

SHC_Component *SHC::getComponent(string *comp_id) {
    if(components.find(*comp_id)!=components.end())
        return components[*comp_id];
    return NULL;
}

void SHC::agglomerateComponents(SHC_Component *c1,SHC_Component *c2) {
    agglomerate(c1,c2);
}

pair<bool, bool> SHC::agglomerate(SHC_Component *c1,SHC_Component *c2,bool add_point) {
    bool r1=false,r2=false;
    SHC_ComponentConnection *cc=NULL;
    if(c1->getParent()!=c2->getParent()) {
        if(c1->isOutlier() && !c1->isObsolete() && !c2->isObsolete()) {
            if(c2->addNewElement(c1->getMean(),this)) {
                c2->addTrace(c1);
                c2->getParent()->addTrace(c1->getParent());
                r1=true;
                removeComponent(c1);
            }
        } else if(c2->isOutlier() && !c1->isObsolete() && !c2->isObsolete()) {
            if(c1->addNewElement(c2->getMean(),this)) {
                c1->addTrace(c2);
                c1->getParent()->addTrace(c2->getParent());
                r2=true;
                removeComponent(c2);
            }
        } else if(!c1->isOutlier() && !c2->isOutlier()) {
            cc=connections.connect(c1, c2, add_point);
            if(cc->getPoints()>=(long)this->sha_agglo_theta && c1->getParent()!=c2->getParent())
                joinComponents(c1, c2);
        }
    } else cc=connections.connect(c1, c2, add_point);
    if(delta && cc) delta->logComponentConnection(cc);
    return make_pair(r1, r2);
}

void SHC::joinComponents(SHC_Component *c1, SHC_Component *c2) {
    unordered_map<string, SHC_Containable*> p1_children=c1->getParent()->fetchChildComponents();
    long p1_elms=0;
    for(auto p1it:p1_children)
        if(typeid(*p1it.second)==typeid(SHC_Component)) p1_elms+=(dynamic_cast<SHC_Component*>(p1it.second))->getElements();
    unordered_map<string, SHC_Containable*> p2_children=c2->getParent()->fetchChildComponents();
    long p2_elms=0;
    for(auto p2it:p2_children)
        if(typeid(*p2it.second)==typeid(SHC_Component)) p2_elms+=(dynamic_cast<SHC_Component*>(p2it.second))->getElements();
    if(p1_elms>p2_elms) {
        c1->getParent()->addTrace(c2->getParent());
        for(auto p2it:p2_children) {
            p2it.second->detachFromParent();
            p2it.second->attachToParent(c1->getParent());
        }
    } else {
        c2->getParent()->addTrace(c1->getParent());
        for(auto p1it:p1_children) {
            p1it.second->detachFromParent();
            p1it.second->attachToParent(c2->getParent());
        }
    }
}

void SHC::pseudoOffline(bool force) {
    if(force || (agglo_count>0 && --agglo_counter<=0)) {
        agglo_counter=agglo_count;
        //set<SHC_Component *> *obs_remove=new set<SHC_Component *>();
        pair<set<string>*,set<string>*> co=getOutliersAndComponents();
        set<SHC_Component *> *exclude=new set<SHC_Component *>();
        for(string testing_comp_id:*co.first) {
            exclude->clear();
            SHC_Component *test_comp=components[testing_comp_id];
            exclude->insert(test_comp);
            if(!test_comp->isObsolete()) {
                // temp out for test
                if(test_comp->isRedirected() && test_comp->getElements()<=0.2*test_comp->getRedirectedComponent()->getElements()) {
                    SHC_Component *super_comp=test_comp->getRedirectedComponent();
                    super_comp->setElements(super_comp->getElements()+test_comp->getElements());
                    incDeltaElementsInDeltaLog(super_comp,test_comp->getElements());
                    super_comp->addTrace(test_comp);
                    removeComponent(test_comp);
                } else if(!test_comp->isRedirected()) {
                    std::chrono::time_point<std::chrono::system_clock> qst=std::chrono::system_clock::now();
                    _int_cr class_res=!sigma_index ? classify(test_comp->getMean(),true,exclude) : classifySigmaIndex(test_comp->getMean(),true,exclude,test_comp);
                    std::chrono::time_point<std::chrono::system_clock> qet=std::chrono::system_clock::now();
                    qTime+=std::chrono::duration_cast<std::chrono::microseconds>(qet-qst).count();
                    if(class_res.comp && !class_res.comp->isBlocked()) {
                        if(!class_res.comp->isOutlier() && test_comp->getElements()<class_res.comp->getElements()) {
                            test_comp->redirectComponent(class_res.comp);
                            agglomerate(test_comp, class_res.comp);
                        }
                    }
                }
            } /*else {
                set<SHC_Component *> *ccomps=connections.getConnectedComponents(test_comp);
                long telms=0;
                bool remove=(ccomps->size()>0 ? false : true),all_neigh_obs=true;
                if(!remove)
                    for(SHC_Component *comp:*ccomps) {
                        if(!comp->isObsolete()) {
                            all_neigh_obs=false;
                            telms+=comp->getElements();
                            if(comp->hasBaseline()) {
                                remove=true;
                                break;
                            }
                        }
                    }
                if(!remove && all_neigh_obs) remove=true;
                if(!remove && telms>driftRemoveCompSizeRatio*test_comp->getElements()) remove=true;
                if(remove || obs_remove->find(test_comp)!=obs_remove->end()) {
                    obs_remove->insert(test_comp);
                    for(SHC_Component *comp:*ccomps) if(comp->isObsolete()) obs_remove->insert(comp);
                }
                delete ccomps;
            }*/
        }
        /*for(SHC_Component *oc:*obs_remove) {
            string oc_id=oc->getId();
            removeComponent(oc);
            if(eventCallback!=NULL) eventCallback(new SHCEvent(AfterObsoleteComponentDeleted,oc_id));
        }*/
        delete exclude;delete co.first;delete co.second;//delete obs_remove;
    }
}

void SHC::processObsoleteComponent(SHC_Component *test_comp) {
    if(!test_comp->isOutlier() && test_comp->isObsolete()) {
        set<SHC_Component *> *ccomps=connections.getConnectedComponents(test_comp),
                             *obs_remove=new set<SHC_Component*>();

        long telms=0;
        bool remove=(ccomps->size()>0 ? false : true),all_neigh_obs=true;
        if(!remove)
            for(SHC_Component *comp:*ccomps) {
                if(!comp->isObsolete()) {
                    all_neigh_obs=false;
                    telms+=comp->getElements();
                    if(comp->hasBaseline()) {
                        remove=true;
                        break;
                    }
                }
            }
        if(!remove && all_neigh_obs) remove=true;
        if(!remove && telms>driftRemoveCompSizeRatio*test_comp->getElements()) remove=true;
        if(remove) {
            obs_remove->insert(test_comp);
            for(SHC_Component *comp:*ccomps) if(comp->isObsolete()) obs_remove->insert(comp);
        }
        delete ccomps;
        for(SHC_Component *oc:*obs_remove) {
            string oc_id=oc->getId();
            removeComponent(oc);
            if(eventCallback!=NULL) eventCallback(new SHCEvent(AfterObsoleteComponentDeleted,oc_id));
        }
        delete obs_remove;
    }
}


pair<set<string>*,set<string>*> SHC::getOutliersAndComponents() {
    set<string> *res_comps=new set<string>(),*res_outs=new set<string>();
    for(auto comp:components)
        if(comp.second->isOutlier()) res_outs->insert(comp.first);
        else res_comps->insert(comp.first);
    return pair<set<string>*,set<string>*>(res_comps, res_outs);
}

void SHC::removeComponent(SHC_Component *c) {
    if(c->isObsolete() && eventCallback!=NULL) eventCallback(new SHCEvent(BeforeObsoleteComponentDeleted,c->getId()));
    if(!c->isOutlier()) {
        set<SHC_Component *> *cc=connections.getConnectedComponents(c);
        for(SHC_Component *cretr:*cc) cretr->addTrace(c);
        delete cc;
    }
    c->removeFromNeighborhood(this);
    c->detachFromParent();
    vector<pair<long,set<SHC_Component *> *>> *partitions=connections.removeComponent(c);
    sort(partitions->begin(), partitions->end(), [=](pair<long,set<SHC_Component *> *>& a,pair<long,set<SHC_Component *> *>& b) {
        return a.first>b.first;
    });
    if(partitions->size()>1) {
        bool biggest=true;
        for(pair<long,set<SHC_Component *> *> p_part:*partitions) {
            if(!biggest) {
                SHC_Containable *cluster=NULL;
                for(SHC_Component *comp:*p_part.second) {
                    SHC_Containable *prev_parent=comp->getParent();
                    pair<string, set<string>> *prev_pair=NULL;
                    if(!cluster) prev_pair=new pair<string, set<string>>(prev_parent->getId(),set<string>(*prev_parent->getTrace()));
                    comp->detachFromParent();
                    if(!cluster) {
                        cluster=new SHC_Containable(containers, comp);
                        if(prev_pair!=NULL) {
                            cluster->addTrace(prev_pair);
                            delete prev_pair;
                        }
                    } else comp->attachToParent(cluster);
                }
            } else biggest=false;
        }
    }
    for(pair<long,set<SHC_Component *> *> p_part:*partitions) delete p_part.second;
    delete partitions;
    if(sigma_index) sigma_index->remove(c);
    if(delta) delta->logComponentRemoval(c);
    components.erase(c->getId());
    delete c;
}

void SHC::print(ostream &o_str) {
    o_str << "**** Statistical Hierarchical Clustering ****" << endl;
    for(auto it=components.begin();it!=components.end();it++)
        it->second->print(o_str);
}

set<string> *SHC::getTopContainers(bool clusters, bool outliers) {
    set<string> *tc=new set<string>();
    for(auto comp_pair:components) {
        if(outliers && comp_pair.second->isOutlier()) {
            SHC_Containable *top=comp_pair.second->fetchTopContainer();
            tc->insert(top->getId());
        }
        if(clusters && !comp_pair.second->isOutlier()) {
            SHC_Containable *top=comp_pair.second->fetchTopContainer();
            tc->insert(top->getId());
        }
    }
    return tc;
}

vector<SHC_Component_Details*> *SHC::getClassificationDetails(string *container_id,double theta,int dim1,int dim2,bool single) {
    vector<SHC_Component_Details *> *res=new vector<SHC_Component_Details *>();
    SHC_Containable *cont=containers->fetchContainer(container_id);
    if(cont!=NULL) {
        unordered_map<string,SHC_Containable *> map=cont->fetchChildComponents();
        for(auto comp:map) {
            if(typeid(*comp.second)==typeid(SHC_Component)) {
                SHC_Component *c1=static_cast<SHC_Component *>(comp.second);
                res->push_back(c1->calculateDetails(theta,dim1,dim2,cbVarianceLimit,cbNLimit,single));
            }
        }
    }
    return res;
}
vector<SHC_Component_Details*> *SHC::getClassificationDetails(string *container_id) {
    return getClassificationDetails(container_id, theta, 0, 1, true);
}

SHC::~SHC() {
    delete virtualVariance;
    delete containers;
    if(sigma_index) delete sigma_index;
}

set<SHC_Component *> *SHC::getUnrelatedComponents(SHC_Component *comp) {
    SHC_Containable *clus=comp->fetchTopContainer();
    set<string> *clusters=getTopContainers();
    set<SHC_Component *> *res_comps=new set<SHC_Component *>();
    for(string clus_id:*clusters) {
        if(clus_id!=clus->getId()) {
            SHC_Containable *cluster=containers->fetchContainer(&clus_id);
            unordered_map<string, SHC_Containable *> child_comps=cluster->fetchChildComponents();
            for(unordered_map<string, SHC_Containable *>::iterator it=child_comps.begin();it!=child_comps.end();it++)
                res_comps->insert(static_cast<SHC_Component *>(it->second));
        }
    }
    delete clusters;
    return res_comps;
}

double SHC::calculateFragmentationIndex() {
    SHC_Component *bc=getBiggestComponent();
    if(bc==NULL) return (double)1;
    return (double)(1-(double)getBiggestComponent()->getElements()/(double)getTotalElements());
}

SHC_Component *SHC::getBiggestComponent() {
    if(components.size()<1) return NULL;
    auto max_it=max_element(components.begin(), components.end(), [=](pair<string,SHC_Component*> a,pair<string,SHC_Component*> b) {
        return a.second->getElements()<b.second->getElements();
    });
    return max_it->second;
}

long SHC::getTotalElements(bool components,bool outliers) {
    long total=0;
    long res=accumulate(this->components.begin(), this->components.end(), total, [=](long a,pair<string,SHC_Component*> b) {
        if(components && !b.second->isOutlier()) return a+b.second->getElements();
        else if(outliers && b.second->isOutlier()) return a+b.second->getElements();
        else return a;
    });
    return res;
}

unordered_map<string, SHC_Component *> *SHC::getComponents() {
    return &components;
}

vector<SHC_Component*> *SHC::getComponents(string *container_id) {
    vector<SHC_Component*> *res=new vector<SHC_Component*>();
    SHC_Containable *clus=containers->fetchContainer(container_id);
    if(clus!=NULL) {
        for(pair<string, SHC_Containable*> it:clus->fetchChildComponents())
            res->push_back(dynamic_cast<SHC_Component*>(it.second));
    }
    return res;
}

vector<SHC_Component*> *SHC::getOutliers() {
    vector<SHC_Component*> *res=new vector<SHC_Component*>();
    for(pair<string,SHC_Component*> it:components)
        if(it.second->isOutlier()) res->push_back(it.second);
    return res;
}

void SHC::merge(SHC *mergedClassifier, SHC_Component *obsoleteComponent, unordered_map<string, std::set<string>*> *front_conn_map) {
    if(mergedClassifier==NULL) return;
    for(auto m_comp:mergedClassifier->components) {
        SHC_Component *trans_comp=m_comp.second->clone(containers,true);
        if(eventCallback) trans_comp->setEventCallback(eventCallback);
        trans_comp->switchDecay(decay,decay_pace);
        //trans_comp->resetBaseline();
        trans_comp->setBaselineLimits(cbVarianceLimit, cbNLimit);
        trans_comp->setDriftOptions(driftCheckingSizeRatio, driftMovementMDThetaRatio);
        components[trans_comp->getId()]=trans_comp;
        if(obsoleteComponent==NULL) {
            string *cluster_id=new string(m_comp.second->getParent()->getId());
            SHC_Containable *cluster=containers->fetchContainer(cluster_id);
            delete cluster_id;
            if(cluster==NULL) cluster=new SHC_Containable(containers,trans_comp);
            else trans_comp->attachToParent(cluster);
        } else {
            SHC_Containable *cluster=obsoleteComponent->getParent();
            trans_comp->attachToParent(cluster);
            connections.connect(trans_comp, obsoleteComponent);
        }
    }
    set<SHC_ComponentConnection*> *mc_cc=mergedClassifier->connections.getConnections();
    for(SHC_ComponentConnection *ccn:*mc_cc)
        connections.connect(components[ccn->getComponent1()->getId()], components[ccn->getComponent2()->getId()]);
    delete mc_cc;
    if(front_conn_map!=NULL) {
        for(auto it:*front_conn_map)
            for(string child_comp_id:*it.second) {
                set<string> *nccid_s=mergedClassifier->traceComponentId(&child_comp_id);
                for(string nccid:*nccid_s) connections.connect(components[it.first], components[nccid]);
                delete nccid_s;
            }
    }
    for(auto m_comp:mergedClassifier->components)
        if(m_comp.second->isRedirected())
            components[m_comp.second->getId()]->redirectComponent(components[m_comp.second->getRedirectedComponent()->getId()]);
    if(sigma_index) { // update sigma index as well
        for(auto m_comp:mergedClassifier->components) sigma_index->create(components[m_comp.first]);
        for(auto m_comp:mergedClassifier->components) {
            SHC_Component *c=components[m_comp.first];
            set<SHC_Component*> *ngs=connections.getConnectedComponents(c);
            unordered_map<SHC_Component*,double> *n=new unordered_map<SHC_Component*,double>();
            for(SHC_Component *nc:*ngs) (*n)[nc]=0;
            delete ngs;
            for(auto m_comp2:mergedClassifier->components)
                if(m_comp.first!=m_comp2.first) (*n)[components[m_comp2.first]]=0;
            sigma_index->update(c,n);
        }
    }
}

SHC::SHC(SHC *cloneFrom):SHC(cloneFrom->theta,cloneFrom->parallelize,cloneFrom->performSharedAgglomeration,cloneFrom->virtualVariance,
                             cloneFrom->agglo_count,cloneFrom->cbVarianceLimit,cloneFrom->cbNLimit) {
    if(cloneFrom->sigma_index) this->sigma_index=cloneFrom->sigma_index->clone();
    this->sha_agglo_theta=cloneFrom->sha_agglo_theta;
    this->componentFormingMinVVRatio=cloneFrom->componentFormingMinVVRatio;
    this->componentBlockingLimitVVRatio=cloneFrom->componentBlockingLimitVVRatio;
    switchDecay(cloneFrom->decay);
    cloned=true;
    // clone containers that are not components
    for(auto m_container:*cloneFrom->containers->getAllContainers()) {
        if(typeid(*m_container.second)!=typeid(SHC_Component)) {
            SHC_Containable *n_container=new SHC_Containable(this->containers);
            n_container->setId(m_container.second->getId());
        }
    }
    // retain relationships among containers
    for(auto m_container:*cloneFrom->containers->getAllContainers()) {
        if(typeid(*m_container.second)!=typeid(SHC_Component)) {
            SHC_Containable *p_container=dynamic_cast<SHC_Containable*>(m_container.second);
            string *container_id=new string(p_container->getId());
            SHC_Containable *nc_container1=this->containers->fetchContainer(container_id);
            delete container_id;
            for(auto ch1:*p_container->getChildren()) {
                if(typeid(*ch1.second)!=typeid(SHC_Component)) {
                    string *ch_container_id=new string(ch1.second->getId());
                    SHC_Containable *nc_container2=this->containers->fetchContainer(ch_container_id);
                    delete ch_container_id;
                    nc_container1->addChild(nc_container2);
                }
            }
        }
    }
    for(auto m_comp:cloneFrom->components) {
        SHC_Component *cloned_comp=m_comp.second->clone(this->containers,true);
        this->components[cloned_comp->getId()]=cloned_comp;
        SHC_Containable *cluster=m_comp.second->getParent();
        string *cluster_id=new string(cluster->getId());
        SHC_Containable *cloned_cluster=this->containers->fetchContainer(cluster_id);
        delete cluster_id;
        cloned_comp->attachToParent(cloned_cluster);
    }
    set<SHC_ComponentConnection *> *ccn_set=cloneFrom->connections.getConnections();
    for(SHC_ComponentConnection *ccn:*ccn_set)
        this->connections.connect(this->components[ccn->getComponent1()->getId()], this->components[ccn->getComponent2()->getId()]);
    delete ccn_set;
    for(auto m_comp:cloneFrom->components)
        if(m_comp.second->isRedirected())
            this->components[m_comp.second->getId()]->redirectComponent(this->components[m_comp.second->getRedirectedComponent()->getId()]);
}

SHC *SHC::clone() {
    SHC *nc=new SHC(theta, parallelize, performSharedAgglomeration, virtualVariance, agglo_count, cbVarianceLimit, cbNLimit);
    if(this->sigma_index) nc->sigma_index=this->sigma_index->clone();
    nc->sha_agglo_theta=this->sha_agglo_theta;
    nc->switchDecay(decay);
    nc->cloned=true;
    // clone containers that are not components
    for(auto m_container:*containers->getAllContainers()) {
        if(typeid(*m_container.second)!=typeid(SHC_Component)) {
            SHC_Containable *n_container=new SHC_Containable(nc->containers);
            n_container->setId(m_container.second->getId());
        }
    }
    // retain relationships among containers
    for(auto m_container:*containers->getAllContainers()) {
        if(typeid(*m_container.second)!=typeid(SHC_Component)) {
            SHC_Containable *p_container=dynamic_cast<SHC_Containable*>(m_container.second);
            string *container_id=new string(p_container->getId());
            SHC_Containable *nc_container1=nc->containers->fetchContainer(container_id);
            delete container_id;
            for(auto ch1:*p_container->getChildren()) {
                if(typeid(*ch1.second)!=typeid(SHC_Component)) {
                    string *ch_container_id=new string(ch1.second->getId());
                    SHC_Containable *nc_container2=nc->containers->fetchContainer(ch_container_id);
                    delete ch_container_id;
                    nc_container1->addChild(nc_container2);
                }
            }
        }
    }
    for(auto m_comp:components) {
        SHC_Component *cloned_comp=m_comp.second->clone(nc->containers,true);
        nc->components[cloned_comp->getId()]=cloned_comp;
        SHC_Containable *cluster=m_comp.second->getParent();
        string *cluster_id=new string(cluster->getId());
        SHC_Containable *cloned_cluster=nc->containers->fetchContainer(cluster_id);
        delete cluster_id;
        cloned_comp->attachToParent(cloned_cluster);
    }
    set<SHC_ComponentConnection *> *ccn_set=connections.getConnections();
    for(SHC_ComponentConnection *ccn:*ccn_set)
        nc->connections.connect(nc->components[ccn->getComponent1()->getId()], nc->components[ccn->getComponent2()->getId()]);
    delete ccn_set;
    for(auto m_comp:components)
        if(m_comp.second->isRedirected())
            nc->components[m_comp.second->getId()]->redirectComponent(nc->components[m_comp.second->getRedirectedComponent()->getId()]);
    return nc;
}

void SHC::setAgglomerationCount(int agglo_count) {
    this->agglo_count=agglo_count;
}

void SHC::setEventCallback(function<void (SHCEvent*)> eventCallback) {
    this->eventCallback=eventCallback;
}

set<string> *SHC::traceComponentId(string *id) {
    set<string> *res=new set<string>();
    if(components.find(*id)!=components.end()) res->insert(*id);
    for(auto comp_it:components) if(comp_it.second->checkTrace(*id)) res->insert(comp_it.first);
    return res;
}

MatrixXd *SHC::getCache() {
    MatrixXd *res=new MatrixXd(_cache.size(),virtualVariance->size());
    for(unsigned i=0;i<_cache.size();i++)
        res->row(i)=_cache.at(i);
    return res;
}
MatrixXd *SHC::getCache(string *id) {
    if(_cache2.find(*id)==_cache2.end()) return NULL;
    MatrixXd *res=new MatrixXd(_cache2[*id].size(),virtualVariance->size());
    for(unsigned i=0;i<_cache2[*id].size();i++)
        res->row(i)=_cache2[*id].at(i);
    return res;
}

double SHC::meanWeightedComponentMDFrom(SHC_Component *fromComponent) {
    double incsum=0,elsum=0;
    for(auto comp:components) {
        if(!comp.second->isOutlier()) {
            incsum+=comp.second->getElements()*fromComponent->mahalanobisDistance(comp.second->getMean());
            //counter++;
            elsum+=comp.second->getElements();
        }
    }
    return elsum==0 ? 0 : incsum/elsum;
}

void SHC::switchDecay(bool decay) {
    this->decay=decay;
}

void SHC::cleanOutliers() {
    set<string> *outliers=new set<string>();
    for(pair<string, SHC_Component*> it:components)
        if(it.second->isOutlier()) outliers->insert(it.first);
    for(string out_id:*outliers) removeComponent(components[out_id]);
    delete outliers;
}

bool SHC::recheckOutlier(string *id) {
    vector<SHC_Component*> *outs=getOutliers();
    for(SHC_Component *c:*outs) if(*id==c->getId()) {
        delete outs;
        return true;
    }
    delete outs;
    if(decayedOutliers.find(*id)!=decayedOutliers.end()) return true;
    return false;
}

void SHC::setSharedAgglomerationThreshold(int sha_agglo_theta) {
    this->sha_agglo_theta=sha_agglo_theta;
}

pair<long,long> SHC::getStatistic() {
    vector<SHC_Component*> *outs=getOutliers();
    long v1=outs->size();
    delete outs;
    return make_pair(components.size()-v1,v1);
}

void SHC::setComponentFormingVVRatio(float ratio) {
    this->componentFormingMinVVRatio=ratio;
}

void SHC::setComponentBlockingLimitVVRatio(float ratio) {
    this->componentBlockingLimitVVRatio=ratio;
}

void SHC::removeSmallComponents(int elementsThreshold) {
    set<string> rem;
    for(pair<string, SHC_Component*> it:components)
        if(it.second->getElements()<elementsThreshold) rem.insert(it.first);
    for(string comp_id:rem) removeComponent(components[comp_id]);
}

void SHC::useSigmaIndex(int neighborhood_mutiplier, bool precision_switch) {
    sigma_index=new SigmaIndex<SHC_Component*>(theta,neighborhood_mutiplier*theta, precision_switch);
}

void SHC::printSigmaIndex(ostream &o_str) {
    if(sigma_index) sigma_index->print(ROOT, o_str);
}

unordered_map<SHC_Component*,double> *_int_cr::getCombinedNeighborhood() {
    unordered_map<SHC_Component*,double> *combo=new unordered_map<SHC_Component*,double>();
    if(classified_map!=NULL)
        for(pair<SHC_Component*,double> it1:*classified_map)
            if(combo->find(it1.first)==combo->end()) (*combo)[it1.first]=it1.second;
    if(neighborhood_map!=NULL)
        for(pair<SHC_Component*,double> it2:*neighborhood_map)
            if(combo->find(it2.first)==combo->end()) (*combo)[it2.first]=it2.second;
    return combo;
}

vector<double> SHC::getTimes() {
    vector<double> times;
    times.push_back(qTime/1000);
    times.push_back(uTime/1000);
    times.push_back(pTime/1000);
    return times;
}

long SHC::getNodeCounter() {
    if(sigma_index) {
        SigmaIndexStatistics *stat=sigma_index->getStatistics();
        long nc=stat->totalCount;
        delete stat;
        return nc;
    } else return this->nodeCounter;
}

SigmaIndex<SHC_Component*> *SHC::getSigmaIndex() {
    return sigma_index;
}

void SHC::consumeDeltaLog(shared_ptr<DeltaLogger> delta_log,string *delta_log_src,shared_ptr<DeltaLogger> amending_log,bool dropCosumationActivity,
                          ostream *o_str) {
    if(dropCosumationActivity && o_str) {
        (*o_str) << "###################################################" << endl;
        (*o_str) << "## Consuming log " << (delta_log_src ? *delta_log_src : "master") << endl;
        (*o_str) << "###################################################" << endl;
    }
    if(amending_log) delta=amending_log;
    else if(!delta_log) delta=make_shared<DeltaLogger>();
    else delta=NULL;
    int total_dropped_points=0;
    for(string rid:delta_log->cr_delta)
        if(components.find(rid)!=components.end()) removeComponent(components[rid]);
    for(pair<string,DL_Component*> it:delta_log->c_delta) {
        string local_id=it.second->id;
        bool existing_component=components.find(local_id)!=components.end();
        set<string> *ts=traceComponentId(&local_id);
        if(!existing_component && ts->size()>0) {
            if(delta) delta->cr_delta.insert(local_id);
            delete ts;
            continue;
        }
        delete ts;
        if(components.find(local_id)==components.end()) {
            if(it.second->outlier) {
                // let us check if this is an outlier on the same place we already have one
                bool existing_outlier=false;
                for(pair<string,SHC_Component*> it_out:components) {
                    if(it_out.second->isOutlier()) {
                        SHC_Component *out=it_out.second;
                        double md_out=out->mahalanobisDistance(it.second->end->mean);
                        if(md_out<(theta/16)) {
                            existing_outlier=true;
                            if(delta) delta->cr_delta.insert(it.second->id);
                            if(dropCosumationActivity && o_str) (*o_str) << "****** it seems that " << local_id << " exists somehow here: " << out->getId() << endl;
                            total_dropped_points++;
                            break;
                        } else if(md_out<theta) {
                            out->addNewElement(it.second->end->mean, this);
                            existing_outlier=true;
                            if(delta) delta->cr_delta.insert(it.second->id);
                            if(dropCosumationActivity && o_str) (*o_str) << "****** we added " << local_id << " to: " << out->getId() << endl;
                            break;
                        }
                    }
                }
                if(existing_outlier) continue;
            }
            // if the new component is really a new component, let us add it
            MatrixXd *cov=it.second->end->isInverted ? new MatrixXd(it.second->end->covariance->inverse()) : new MatrixXd(*it.second->end->covariance);
            long de=0;
            if(delta_log_src) {
                if(it.second->delta_elements.find(*delta_log_src)!=it.second->delta_elements.end()) de=it.second->delta_elements[*delta_log_src];
            } else {
                for(pair<string,long> itx:it.second->delta_elements)
                    if(itx.first!=deltaLoggingSourceName) de+=itx.second;
            }
            SHC_Component *nc=new SHC_Component(containers,it.second->end->mean,cov,it.second->end->isInverted,de,virtualVariance);
            nc->setId(it.second->id);
            nc->addTrace(&it.second->trace);
            if(delta_log_src) nc->setSourceNode(*delta_log_src);
            if(SHC_Containable *parent=containers->fetchContainer(&it.second->parent_id)) nc->attachToParent(parent);
            else {
                SHC_Containable *cluster=new SHC_Containable(containers, nc);
                cluster->setId(it.second->parent_id);
            }
            delete cov;
            nc->switchDecay(decay,decay_pace);
            if(eventCallback) nc->setEventCallback(eventCallback);
            nc->setObsolete(it.second->obsolete);
            nc->setBlocked(it.second->blocked);
            nc->setBaselineLimits(cbVarianceLimit,cbNLimit);
            components[nc->getId()]=nc;
            if(delta) {
                if(delta_log_src) {
                    delta->logComponent(nc,*delta_log_src,de);
                } else {
                    for(pair<string,long> itx:it.second->delta_elements)
                        if(itx.first!=deltaLoggingSourceName)
                            delta->logComponent(nc,itx.first,itx.second);
                }
            }
        } else {
            SHC_Component *c=components[local_id];
            if(c->getParent()->getId()!=it.second->parent_id) {
                c->detachFromParent();
                if(SHC_Containable *parent=containers->fetchContainer(&it.second->parent_id)) c->attachToParent(parent);
                else {
                    SHC_Containable *cluster=new SHC_Containable(containers, c);
                    cluster->setId(it.second->parent_id);
                }
            }
            if(c->isOutlier() && it.second->outlier) continue;
            long ce=c->getElements(),cd=it.second->end->elements;
            if(delta_log_src) {
                double ce_coeff=((double)ce/((double)ce+(double)cd));
                VectorXd mean_delta=((*it.second->end->mean)-(*c->getMean()))*ce_coeff;
                mean_delta+=*c->getMean();
                c->setMean(new VectorXd(mean_delta));
                MatrixXd *cov=c->getCovariance();
                for(int i=0;i<cov->rows();i++) {
                    for(int j=0;j<cov->cols();j++) {
                       if(abs((*cov)(i,j))<abs((*it.second->end->covariance)(i,j)))
                            (*cov)(i,j)=(*it.second->end->covariance)(i,j);
                    }
                }
                c->setCovariance(cov,c->isCovarianceInverted());
                delete cov;
            } else {
                c->setMean(new VectorXd(*it.second->end->mean));
                c->setCovariance(it.second->end->covariance,it.second->end->isInverted);
            }
            
            c->setObsolete(it.second->obsolete);
            c->setBlocked(it.second->blocked);
            c->setBaselineLimits(cbVarianceLimit,cbNLimit);
            c->addTrace(&it.second->trace);
            if(delta_log_src) {
                long de=0;
                if(it.second->delta_elements.find(*delta_log_src)!=it.second->delta_elements.end()) de=it.second->delta_elements[*delta_log_src];
                SHC_Component *temp=c;
                while(temp->isRedirected()) temp=temp->getRedirectedComponent();
                temp->setElements(ce+de);
                if(delta) delta->logComponent(temp,*delta_log_src,de);
            } else {
                c->setElements(it.second->end->elements);
            }
        }
    }
    
    //for (SHC_Component *c:touched_components) {
    for (pair<string,SHC_Component*> it:components) {
        SHC_Component *c=it.second;
        if(delta_log->c_delta.find(c->getId())!=delta_log->c_delta.end()) {
            DL_Component *dl_c=delta_log->c_delta[c->getId()];
            if(dl_c->redirectedTo) {
                string r_to_id=*dl_c->redirectedTo;
                if(components.find(r_to_id)!=components.end()) {
                    SHC_Component *r_c=components[r_to_id];
                    if(!c->isRedirected() || c->getRedirectedComponent()!=r_c) {
                        c->redirectComponent(r_c);
                        if(c->getParent()!=r_c->getParent()) {
                            if(dropCosumationActivity && o_str) (*o_str) << "*** join components " << c->getId() << " " << r_c->getId() << endl;
                            if(delta) {
                                delta->logComponent(c,deltaLoggingSourceName);
                                delta->logComponent(r_c,deltaLoggingSourceName);
                            }
                            joinComponents(c, r_c);
                        }
                    }
                }
            }
            for(string o_n:dl_c->neighborhood) {
                if(components.find(o_n)!=components.end()) {
                    SHC_Component *n_o_c=components[o_n];
                    c->addNeighbor(n_o_c);
                }
            }
        }
        
        if(delta_log_src && !c->isOutlier()) {
            set<SHC_Component*> ex={c};
            std::chrono::time_point<std::chrono::system_clock> qst=std::chrono::system_clock::now();
            _int_cr class_res=!sigma_index ? classify(c->getMean(), true, &ex) : classifySigmaIndex(c->getMean(), true, &ex, c);
            std::chrono::time_point<std::chrono::system_clock> qet=std::chrono::system_clock::now();
            qTime+=std::chrono::duration_cast<std::chrono::microseconds>(qet-qst).count();
            for(pair<SHC_Component*,double> it:*class_res.neighborhood_map)
                if(it.first->isOutlier()) c->addNeighbor(it.first);
            for(pair<SHC_Component*,double> it:*class_res.classified_map) {
                if(!it.first->isOutlier()) {
                    if(c->getParent()!=it.first->getParent()) {
                        if(dropCosumationActivity && o_str) (*o_str) << "*** join components " << c->getId() << " " << it.first->getId() << endl;
                        if(delta) {
                            delta->logComponent(c,deltaLoggingSourceName);
                            delta->logComponent(it.first,deltaLoggingSourceName);
                        }
                        joinComponents(c, it.first);
                    }
                } else {
                    if(dropCosumationActivity && o_str) (*o_str) << "*** agglomerate " << c->getId() << " " << it.first->getId() << endl;
                    agglomerate(c, it.first);
                }
            }
            if(!c->isRedirected()) {
                bool redirected=false;
                for(pair<SHC_Component*,double> it:*class_res.classified_map) {
                    if(!it.first->isOutlier()) {
                        if(c->getElements()<=it.first->getElements()) {
                            if(dropCosumationActivity && o_str) (*o_str) << "*** redirect " << c->getId() << " to " << it.first->getId() << endl;
                            c->redirectComponent(it.first);
                            redirected=true;
                            break;
                        }
                    }
                }
            }
            c->agglomerateNeighborhood(this);
        }
    }
    
    
    for(pair<SHC_ComponentConnectionId<string, string>, DL_ComponentConnection*> it:delta_log->cc_delta) {
        if(components.find(it.second->c1_id)!=components.end() &&
           components.find(it.second->c2_id)!=components.end()) {
            SHC_Component *c1=components[it.second->c1_id],*c2=components[it.second->c2_id];
            SHC_ComponentConnection *cc=connections.connect(c1, c2, false);
            cc->addPoints(it.second->points);
            agglomerate(c1, c2, false);
        }
    }
    pseudoOffline(true);
    if(delta) {
        for(pair<string,SHC_Component*> it:components) delta->finalizeComponent(it.second);
        delta=NULL;
    }
    if(dropCosumationActivity && o_str) (*o_str) << "$$$$ Total dropped points " << total_dropped_points << endl;
}

void SHC::incDeltaElementsInDeltaLog(SHC_Component *comp,int number) {
    if(delta) delta->addComponentDeltaElements(comp,deltaLoggingSourceName,number);
}

void SHC::setDeltaLoggingSourceName(string s) {
    this->deltaLoggingSourceName=s;
}

string SHC::getDeltaLoggingSourceName() {
    return this->deltaLoggingSourceName;
}

void SHC::clearEigenMPSupport() {
    Eigen::setNbThreads(1);
}
