#include "SHC_Container.hpp"
#include "SHC_Utils.hpp"
#include "SHC_Component.hpp"
#include <memory>
#include <iostream>
using namespace std;

SHC_Containable::SHC_Containable(SHC_Containable_Set *set) {
    id=generate_hex(10);
    if(set!=NULL) {
        this->set=set;
        if(typeid(*this)==typeid(SHC_Containable)) this->set->addContainer(this);
    }
}

SHC_Containable::SHC_Containable(SHC_Containable_Set *set, SHC_Containable *initChild):SHC_Containable(set) {
    addChild(initChild);
}

SHC_Containable::~SHC_Containable() {
    if(set!=NULL) set->removeContainer(&id);
}

string SHC_Containable::getId() {
    return id;
}

SHC_Containable *SHC_Containable::getParent() {
    return parent;
}

void SHC_Containable::setParent(SHC_Containable *parent) {
    this->parent=parent;
}

SHC_Containable *SHC_Containable::findTop() {
    SHC_Containable *x=this;
    while(x->getParent()!=NULL)
        x=x->getParent();
    return x;
}

void SHC_Containable::addChild(SHC_Containable *child) {
    if(children.find(child->getId())==children.end()) {
        container_m.lock();
        children[child->getId()]=child;
        if(child->getParent()!=NULL)
            child->getParent()->removeChild(child);
        child->setParent(this);
        container_m.unlock();
    }
}

void SHC_Containable::removeChild(SHC_Containable *child) {
    if(children.find(child->getId())!=children.end()) {
        container_m.lock();
        children.erase(child->getId());
        child->setParent(NULL);
        container_m.unlock();
    }
}

void SHC_Containable::detachFromParent() {
    if(parent!=NULL) {
        container_m.lock();
        SHC_Containable *temp=parent;
        temp->removeChild(this);
        if(temp->children.size()==0) {
            temp->detachFromParent();
            delete temp;
        }
        container_m.unlock();
    }
}

void SHC_Containable::attachToParent(SHC_Containable *parent) { 
    detachFromParent();
    container_m.lock();
    parent->addChild(this);
    container_m.unlock();
}

unordered_map<string, SHC_Containable *> SHC_Containable::fetchChildComponents() {
    unordered_map<string, SHC_Containable *> res;
    for(auto child:children) {
        if(typeid(*child.second)==typeid(SHC_Component)) {
            SHC_Component *child_comp=static_cast<SHC_Component *>(child.second);
            unordered_map<string, SHC_Containable *> t1=child_comp->fetchChildComponents();
            res.insert(t1.begin(),t1.end());
        } else {
            unordered_map<string, SHC_Containable *> t1=child.second->fetchChildComponents();
            res.insert(t1.begin(),t1.end());
        }
    }
    return res;
}

double SHC_Containable::calculateSeparation(SHC_Containable *cont, double theta) {
    unordered_map<string, SHC_Containable *> comps1=this->fetchChildComponents(), comps2=cont->fetchChildComponents();
    SHC_Component *minc1=NULL, *minc2=NULL;
    double *min=NULL;
    for(auto comp1:comps1) {
        SHC_Component *c1=static_cast<SHC_Component *>(comp1.second);
        for(auto comp2:comps2) {
            SHC_Component *c2=static_cast<SHC_Component *>(comp2.second);
            double emesaure=c1->euclideanDistance(c2);
            if(min==NULL || emesaure<*min) {
                min=new double(emesaure);
                minc1=c1;
                minc2=c2;
            }
        }
    }
    delete min;
    return minc1->connectionMeasure(minc2, theta);
}

SHC_Containable *SHC_Containable::fetchTopContainer() {
    if(parent!=NULL) return parent->fetchTopContainer();
    else return this;
}

bool SHC_Containable::hasChildren() {
    return children.size()>0;
}

void SHC_Containable_Set::addContainer(SHC_Containable *container) {
    if(_set.find(container->getId())==_set.end()) _set[container->getId()]=container;
}
void SHC_Containable_Set::addComponent(SHC_Component *comp) {
    if(_set.find(comp->getId())==_set.end()) _set[comp->getId()]=comp;
}

void SHC_Containable_Set::removeContainer(string *id) {
    _set.erase(*id);
}

SHC_Containable_Set::~SHC_Containable_Set() {
    set<string> *cs=new set<string>();
    for(pair<string,SHC_Containable*> cont_it:_set) cs->insert(cont_it.first);
    for(string c_id:*cs) {
        SHC_Containable *cont=fetchContainer(&c_id);
        if(cont!=NULL) delete cont;
    }
    delete cs;
}

unordered_map<string, SHC_Containable *> *SHC_Containable_Set::getAllContainers() {
    return &_set;
}

SHC_Containable *SHC_Containable_Set::fetchContainer(string *id) {
    if(_set.find(*id)!=_set.end()) return _set[*id];
    return NULL;
}

void SHC_Containable::forcesetId(string id) {
    this->id=id;
}

void SHC_Containable::setId(string id) {
    if(set!=NULL) set->removeContainer(&this->id);
    forcesetId(id);
    if(set!=NULL) set->addContainer(this);
}

unordered_map<std::string, SHC_Containable *> *SHC_Containable::getChildren() {
    return &children;
}

void SHC_Containable::addTrace(SHC_Containable *previous) {
    trace.insert(previous->trace.begin(),previous->trace.end());
    trace.insert(previous->getId());
}

void SHC_Containable::addTrace(pair<string, std::set<string>> *prev_pair) {
    trace.insert(prev_pair->second.begin(),prev_pair->second.end());
    trace.insert(prev_pair->first);
}

void SHC_Containable::addTrace(std::set<string> *prev_log) {
    trace.insert(prev_log->begin(),prev_log->end());
}

bool SHC_Containable::checkTrace(string id) {
    return trace.find(id)!=trace.end();
}

set<string> *SHC_Containable::getTrace() {
    return &trace;
}
