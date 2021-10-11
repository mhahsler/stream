#include "SHC_DeltaLogger.hpp"

DL_Component *DeltaLogger::logComponent(SHC_Component *logged_comp,string source,int init_delta_elements) {
    DL_Component *c=NULL;
    string id=logged_comp->getId();
    if(c_delta.find(id)==c_delta.end()) {
        c=new DL_Component(id,logged_comp->getParent()->getId());
        c->source=source;
        c->delta_elements[source]=init_delta_elements;
        if(!logged_comp->isOutlier()) c->start=new DL_Component_SimpleData(logged_comp);
        else c->created=true;
        c_delta[id]=c;
    } else {
        c=c_delta[id];
        if(c->delta_elements.find(source)==c->delta_elements.end())            
            c->delta_elements[source]=init_delta_elements;
    }
    return c;
}

void DeltaLogger::addComponentDeltaElements(SHC_Component *logged_comp,string source,int number) {
    DL_Component *c=logComponent(logged_comp,source);
    c->delta_elements[source]=c->delta_elements[source]+number;
}

void DeltaLogger::finalizeComponent(SHC_Component *logged_comp) {
    DL_Component *c=NULL;
    if(c_delta.find(logged_comp->getId())!=c_delta.end()) {
        c=c_delta[logged_comp->getId()];
        if(c->end) delete c->end;
        c->end=new DL_Component_SimpleData(logged_comp);
        if(logged_comp->isRedirected()) {
            if(c->redirectedTo) delete c->redirectedTo;
            c->redirectedTo=new string(logged_comp->getRedirectedComponent()->getId());
        }
        if(logged_comp->hasBaseline()) c->baseline=new DL_Component_SimpleData(logged_comp->getBaseline());
        for(SHC_Component *o:*logged_comp->getNeighborhood()) c->neighborhood.insert(o->getId());
        for(string t_id:*logged_comp->getTrace()) c->trace.insert(t_id);
        c->outlier=logged_comp->isOutlier();
        c->obsolete=logged_comp->isObsolete();
        c->blocked=logged_comp->isBlocked();
        c->parent_id=logged_comp->getParent()->getId();
    }
}

void DeltaLogger::logComponentConnection(SHC_ComponentConnection *logged_cc) {
    DL_ComponentConnection *cc=NULL;
    string id1=logged_cc->getComponent1()->getId(),id2=logged_cc->getComponent2()->getId();
    if(cc_delta.find({id1,id2})!=cc_delta.end())
        cc=cc_delta[{id1,id2}];
    else {
        cc=new DL_ComponentConnection(logged_cc);
        cc_delta[{id1,id2}]=cc;
    }
    ++cc->points;
}

void DeltaLogger::logComponentRemoval(SHC_Component *removed_comp) {
    string rid=removed_comp->getId();
    cr_delta.insert(rid);
    for(pair<string,DL_Component*> it:c_delta) {
        if(it.second->redirectedTo && *it.second->redirectedTo==rid) {
            delete it.second->redirectedTo;
            it.second->redirectedTo=NULL;
        }
        it.second->neighborhood.erase(rid);
    }
    if(c_delta.find(rid)!=c_delta.end()) {
        delete c_delta[rid];
        c_delta.erase(rid);
    }
    vector<SHC_ComponentConnectionId<string, string>> removals;
    for(pair<SHC_ComponentConnectionId<string, string>,DL_ComponentConnection*> it:cc_delta) {
        if(it.second->c1_id==rid || it.second->c2_id==rid)
            removals.push_back(it.first);
    }
    for(SHC_ComponentConnectionId<string, string> it:removals) {
        delete cc_delta[it];
        cc_delta.erase(it);
    }
}

DeltaLogger::~DeltaLogger() {
    for(pair<string,DL_Component*> it:c_delta)
        delete it.second;
    for(pair<SHC_ComponentConnectionId<string, string>,DL_ComponentConnection*> it:cc_delta)
        delete it.second;
}

void DeltaLogger::print(ostream &o_str,string source) {
    o_str << "+++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    o_str << "++ Delta log " << source << endl;
    o_str << "+++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    unordered_map<string,long> parents,totals;
    long total_end=0;
    for(pair<string,DL_Component*> it1:c_delta) {
        for(pair<string,long> it2:it1.second->delta_elements) {
            if(totals.find(it2.first)==totals.end()) totals[it2.first]=it2.second;
            else totals[it2.first]=totals[it2.first]+it2.second;
        }
        if(parents.find(it1.second->parent_id)==parents.end()) parents[it1.second->parent_id]=it1.second->end->elements;
        else parents[it1.second->parent_id]=parents[it1.second->parent_id]+it1.second->end->elements;
        o_str << "Add/Update " << it1.first << " ";
        o_str << "total:" << it1.second->end->elements << " ";
        for(pair<string,long> it3:it1.second->delta_elements)
            o_str << it3.first << ":" << it3.second << " ";
        o_str << endl;
        o_str << "\t\t rto:" << (it1.second->redirectedTo ? *it1.second->redirectedTo : "none") << " pid:" << it1.second->parent_id << " src:" << it1.second->source << endl;
        total_end+=it1.second->end->elements;
    }
    for(string it2:cr_delta) {
        o_str << "Remove " << it2 << endl;
    }
    for(pair<string,long> it:parents) {
        o_str << "Parent " << it.first << " elements " << it.second << endl;
    }
    long total=0;
    for(pair<string,long> it:totals) {
        o_str << "Source " << it.first << " points " << it.second << endl;
        total+=it.second;
    }
    o_str << "Total delta points " << total << endl;
    o_str << "Total final points " << total_end << endl;
    o_str << "--- end delta log print" << endl;
}


