#include "SHC_ComponentConnection.hpp"
#include <unordered_map>
#include <set>
#include "SHC_Utils.hpp"
using namespace std;

SHC_ComponentConnection::SHC_ComponentConnection(SHC_Component *c1, SHC_Component *c2) {
    this->id=generate_hex(10);
    this->c1=c1;
    this->c2=c2;
}

void SHC_ComponentConnection::addPoint() {
    points++;
}

bool SHC_ComponentConnection::operator==(const SHC_ComponentConnection &cc) const {
    return id==cc.id;
}

SHC_Component *SHC_ComponentConnection::getConnectedComponent(SHC_Component *c) {
    if(*c==*c1) return c2;
    else return c1;
}

SHC_ComponentConnection *SHC_ComponentConnection_Set::connect(SHC_Component *c1, SHC_Component *c2, bool add_point) {
    SHC_ComponentConnection *cc=NULL;
    if(!isConnected(c1, c2, true)) {
        cc=new SHC_ComponentConnection(c1, c2);
        if(!add_point) cc->setPoints(0);
        _set[{c1->getId(), c2->getId()}]=cc;
    } else {
        cc=_set[{c1->getId(), c2->getId()}];
        if(add_point) cc->addPoint();
    }
    if(_links.find(c1->getId())==_links.end()) _links[c1->getId()]=new set<SHC_Component *>();
    _links[c1->getId()]->insert(c2);
    if(_links.find(c2->getId())==_links.end()) _links[c2->getId()]=new set<SHC_Component *>();
    _links[c2->getId()]->insert(c1);
    return cc;
}

bool SHC_ComponentConnection_Set::isConnected(SHC_Component *c1, SHC_Component *c2, bool directly) {
    if(_set.find({c1->getId(), c2->getId()})!=_set.end()) return true;
    if(!directly) {
        set<SHC_Component *> *_cs=new set<SHC_Component *>();
        _getConnectedSet(c1, _cs);
        bool res=_cs->find(c2)!=_cs->end();
        delete _cs;
        return res;
    }
    return false;
}

set<SHC_Component *> *SHC_ComponentConnection_Set::getConnectedComponents(SHC_Component *c) {
    set<SHC_Component *> *res=new set<SHC_Component *>();
    if(_links.find(c->getId())!=_links.end()) {
        set<SHC_Component *> *tmp=_links[c->getId()];
        res->insert(tmp->begin(), tmp->end());
    }
    return res;
}

SHC_Component *SHC_ComponentConnection_Set::getMaxConnectedComponents(SHC_Component *c) {
    set<SHC_Component *> *cc=getConnectedComponents(c);
    long biggest=0;
    SHC_Component *res=NULL;
    for(SHC_Component *c:*cc) {
        if(c->getElements()>biggest) {
            biggest=c->getElements();
            res=c;
        }
    }
    delete cc;
    return res;
}

vector<pair<long,set<SHC_Component *> *>> *SHC_ComponentConnection_Set::removeComponent(SHC_Component *c) {
    set<SHC_Component*> *_in_set=new set<SHC_Component*>();
    _getConnectedSet(c, _in_set);
    if(_in_set->find(c)!=_in_set->end()) _in_set->erase(c);
    
    set<SHC_Component *> *ccomps=getConnectedComponents(c);
    set<SHC_Component*> *cc_set=_links[c->getId()];
    _links.erase(c->getId());
    delete cc_set;
    for(SHC_Component *ccomp:*ccomps) {
        if(_set.find({c->getId(), ccomp->getId()})!=_set.end()) {
            SHC_ComponentConnection *cc=_set[{c->getId(), ccomp->getId()}];
            _set.erase({c->getId(), ccomp->getId()});
            delete cc;
        }
        if(_links.find(ccomp->getId())!=_links.end())
            _links[ccomp->getId()]->erase(c);
    }
    
    vector<pair<long,set<SHC_Component *> *>> *res=calculatePartitions(_in_set);
    delete _in_set;delete ccomps;
    return res;
}

void SHC_ComponentConnection_Set::_getConnectedSet(SHC_Component *_from_c, set<SHC_Component *> *_in_set) {
    set<SHC_Component *> *_d_c=getConnectedComponents(_from_c);
    for(auto cit:*_d_c) {
        if(_in_set->find(cit)==_in_set->end()) {
            _in_set->insert(cit);
            _getConnectedSet(cit, _in_set);
        }
    }
    delete _d_c;
}

vector<pair<long,set<SHC_Component *> *>> *SHC_ComponentConnection_Set::calculatePartitions(set<SHC_Component *> *_in_set) {
    vector<pair<long,set<SHC_Component *> *>> *res=new vector<pair<long,set<SHC_Component *> *>>();
    while(_in_set->size()>0) {
        set<SHC_Component *> *_part=new set<SHC_Component *>();
        auto elem=_in_set->begin();
        _part->insert(*elem);
        _in_set->erase(*elem);
        while(_partition2(_part, _in_set));
        long tel=0;
        for(SHC_Component *pc:*_part) tel+=pc->getElements();
        res->push_back(pair<long, set<SHC_Component *> *>(tel, _part));
    }
    return res;
}

bool SHC_ComponentConnection_Set::_partition2(set<SHC_Component *> *_is1, set<SHC_Component *> *_is2) {
    for(SHC_Component *c1:*_is1) {
        for(SHC_Component *c2:*_is2) {
            if(isConnected(c1, c2)) {
                _is1->insert(c2);
                _is2->erase(c2);
                return true;
            }
        }
    }
    return false;
}

set<SHC_ComponentConnection *> *SHC_ComponentConnection_Set::getConnections() {
    set<SHC_ComponentConnection *> *res=new set<SHC_ComponentConnection *>();
    for(auto it:_set)
        res->insert(it.second);
    return res;
}

SHC_Component *SHC_ComponentConnection::getComponent1() {
    return c1;
}

SHC_Component *SHC_ComponentConnection::getComponent2() {
    return c2;
}

long SHC_ComponentConnection::getPoints() {
    return this->points;
}

void SHC_ComponentConnection::setPoints(long v) {
    this->points=v;
}

void SHC_ComponentConnection::addPoints(long v) {
    this->points+=v;
}

SHC_ComponentConnection_Set::~SHC_ComponentConnection_Set() {
    for(auto cc_it:_set) delete cc_it.second;
    for(auto lks_it:_links) delete lks_it.second;
}
