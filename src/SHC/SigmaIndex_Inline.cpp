#include "SigmaIndex.hpp"

/**
 Sigma-Index node constructor.
 @param population A population object for this node.
 */
template<class T>
SigmaIndexNode<T>::SigmaIndexNode(T population) {
    this->pop_ptr=population;
    this->bckp_id=population->getId();
}
/**
 A constructor for the ROOT node.
 */
template<class T>
SigmaIndexNode<T>::SigmaIndexNode() {
    this->bckp_id=ROOT;
}
/**
 Sigma-Index node desctructor. We do not clean population objects here, as we do not know how these populations were instances.
 For example: a population can be an SHC component, which, if deleted here, would distrupt the list of SHC concepts.
 */
template<class T>
SigmaIndexNode<T>::~SigmaIndexNode() {
}
/**
 Returns Sigma-Index node identifier.
 @return Node identifier.
 */
template<class T>
string SigmaIndexNode<T>::getId() {
    return bckp_id;
}
/**
 Returns Sigma-Index node population object.
 @return Population object.
 */
template<class T>
T SigmaIndexNode<T>::getPopulation() {
    return this->pop_ptr;
}

/**
 Add child to this node.
 @param child The child node to add.
 */
template<class T>
void SigmaIndexNode<T>::addChild(SigmaIndexNode<T> *child) {
    if(!isDirectChild(child)) {
        children[child->getId()]=child;
        if(!child->isDirectParent(this))
            child->parents[getId()]=this;
    }
}
/**
 Removes a child from this node.
 @param child The child node to remove.
 */
template<class T>
void SigmaIndexNode<T>::removeChild(SigmaIndexNode<T> *child) {
    if(isDirectChild(child)) {
        children.erase(child->getId());
        if(child->isDirectParent(this))
            child->parents.erase(this->getId());
    }
}
/**
 Tests whether the supplied node is a direct child of this node.
 @param node The tested node.
 @return false - not a direct child, true - is a direct child
 */
template<class T>
bool SigmaIndexNode<T>::isDirectChild(SigmaIndexNode<T> *node) {
    return children.find(node->getId())!=children.end();
}
/**
 Tests whether the supplied node is a direct parent of this node.
 @param node The tested node.
 @return false - not a direct parent, true - is a direct parent
 */
template<class T>
bool SigmaIndexNode<T>::isDirectParent(SigmaIndexNode<T> *node) {
    return parents.find(node->getId())!=parents.end();
}
/**
 Tests whether this node is direct child of the ROOT node.
 @return false - not a direct child, true - is a direct child
 */
template<class T>
bool SigmaIndexNode<T>::underTheRoot() {
    return parents.find(ROOT)!=parents.end();
}
/**
 Returns child nodes.
 @return A map containing all child nodes.
 */
template<class T>
unordered_map<string, SigmaIndexNode<T>*> *SigmaIndexNode<T>::getChildren() {
    return &children;
}
/**
 Returns parent nodes.
 @return A map containing all parent nodes.
 */
template<class T>
unordered_map<string, SigmaIndexNode<T>*> *SigmaIndexNode<T>::getParents() {
    return &parents;
}
/**
 Returns a vector of parent nodes.
 @return A vector of all parent nodes. This vector is a working instance containing only pointers to nodes.
 */
template<class T>
vector<SigmaIndexNode<T>*> *SigmaIndexNode<T>::getParentsVector() {
    vector<SigmaIndexNode*> *res=new vector<SigmaIndexNode*>();
    for(pair<string,SigmaIndexNode*> it:parents) res->push_back(it.second);
    return res;
}
/**
 Returns a vector of child nodes.
 @return A vector of all child nodes. This vector is a working instance containing only pointers to nodes.
 */
template<class T>
vector<SigmaIndexNode<T>*> *SigmaIndexNode<T>::getChildrenVector() {
    vector<SigmaIndexNode*> *res=new vector<SigmaIndexNode*>();
    for(pair<string,SigmaIndexNode*> it:children) res->push_back(it.second);
    return res;
}
/**
 Returns a vector of child nodes, sorted descending by population size.
 @return A vector of all child nodes, sorted descending by population size. This vector is a working instance containing only pointers to nodes.
 */
template<class T>
vector<SigmaIndexNode<T>*> *SigmaIndexNode<T>::getSortedChildren() {
    vector<SigmaIndexNode<T>*> *res=new vector<SigmaIndexNode<T>*>();
    for(pair<string, SigmaIndexNode<T>*> it:children)
        res->push_back(it.second);
    sort(res->begin(), res->end(), [](SigmaIndexNode<T> *a, SigmaIndexNode<T> *b) {
        if(a==NULL || a->getPopulation()==NULL || b==NULL || b->getPopulation()==NULL) return false;
        return a->getPopulation()->getElements()>b->getPopulation()->getElements();
    });
    return res;
}
/**
 Returns a vector of parent nodes, sorted descending by population size.
 @return A vector of all parent nodes, sorted descending by population size. This vector is a working instance containing only pointers to nodes.
*/
template<class T>
vector<SigmaIndexNode<T>*> *SigmaIndexNode<T>::getSortedParents() {
    vector<SigmaIndexNode<T>*> *res=new vector<SigmaIndexNode<T>*>();
    for(pair<string, SigmaIndexNode<T>*> it:parents)
        res->push_back(it.second);
    sort(res->begin(), res->end(), [](SigmaIndexNode<T> *a, SigmaIndexNode<T> *b) {
        if(a==NULL || a->getPopulation()==NULL || b==NULL || b->getPopulation()==NULL) return false;
        return a->getPopulation()->getElements()>b->getPopulation()->getElements();
    });
    return res;
}
/**
 Removes the ROOT node from the parents node.
 @param root_node A pointer to the ROOT node.
 */
template<class T>
void SigmaIndexNode<T>::removeRootParent(SigmaIndexNode<T> *root_node) {
    root_node->removeChild(this);
}
/**
 Removes this node from the current parents and moves it under the specified node.
 @param pn A node that must become an unique parent of this node.
 */
template<class T>
void SigmaIndexNode<T>::moveTo(SigmaIndexNode<T> *pn) {
    if(parents.size()>0)
        for(pair<string, SigmaIndexNode<T> *> it:parents) it.second->removeChild(this);
    pn->addChild(this);
}
/**
 Operator that can be used to test whether two pointers point to the same node
 @param c Pointer to the tested node.
 @return Returns true if this node and the tested node are the same one.
 */
template<class T>
bool SigmaIndexNode<T>::operator==(const SigmaIndexNode<T> &c) const {
    return this->pop_ptr==c.pop_ptr;
}
/**
 Operator that can be used to test whether two pointers DO NOT point to the same node
 @param c Pointer to the tested node.
 @return Returns true if this node and the tested node are NOT the same one.
 */
template<class T>
bool SigmaIndexNode<T>::operator!=(const SigmaIndexNode<T> &c) const {
    return this->pop_ptr!=c.pop_ptr;
}

/**
 Sigma-Index constructor.
 @param theta Classification threshold.
 @param neighborhood_theta Neighborhood threshold.
 @param precision_switch Keep the precision high (lowers the speed for some cases)
 */
template<class U>
SigmaIndex<U>::SigmaIndex(double theta, double neighborhood_theta, bool precision_switch) {
    this->theta=theta;
    this->ntheta=neighborhood_theta;
    this->switch1=precision_switch;
    SigmaIndexNode<U> *root_node=new SigmaIndexNode<U>();
    nodes[ROOT]=root_node; // Immediatelly create the ROOT node
}
/**
 Sigma-Index destructor.
 */
template<class U>
SigmaIndex<U>::~SigmaIndex() {
    for(pair<string, SigmaIndexNode<U> *> it:nodes)
        delete it.second; // clean nodes
}

/**
 Incremental update of the Sigma-Index. We can perform such update right after the query.
 This kind of update prevents re-calculation of the neighborhood, and by this saving some Mahalanobis distance calculations.
 Template U represents the population class.
 @param classified The classified population. The input data point was found to belong to this population.
 @param neighborhood A set of populations that was found to be a neighborhood of the input data point, and by this the neighborhood of the classified population.
 */
template<class U>
void SigmaIndex<U>::update(U classified,unordered_map<U,double> *neighborhood) {
    if(classified==NULL) return;
    SigmaIndexNode<U> *class_node=nodes[classified->getId()]; // search for the classified node... node is just a wrapper for a population
    if(class_node->getParents()->size()>0) { // if classified node has parents (usually has)
        /*
         This chunk of the code checks whether the child became bigger than the parent
         If that is the case, we reverse the direction of the edge
         */
        vector<SigmaIndexNode<U>*> *v=class_node->getParentsVector(); // get the working vector of these parent
        for(SigmaIndexNode<U> *pn:*v) { // iterate through them
            if(pn->getId()!=ROOT && pn->getPopulation()->getElements()<class_node->getPopulation()->getElements() &&
               pn->isDirectChild(class_node)) { // if the parent is not ROOT and the population size of the parent became smaller
                disconnect(pn, class_node); // reverse direction of the edge
                connect(class_node, pn);
            }
        }
        delete v;
    }
    if(neighborhood!=NULL || neighborhood->size()>0) {
        /*
         If there was some neighborhood detected in the previous query. This is THE incremental update.
         Effectively, we update only this neighborhood given by the user... which was (ideally) calculated in the previous query.
         */
        for(pair<U,double> nbor:*neighborhood) { // iterate through the neighborhood
            if(nbor.first!=classified) { // if the neighbor population is not the same as the classified population
                SigmaIndexNode<U> *neigh_node=nodes[nbor.first->getId()]; // find the corresponding node
                make_connections(class_node, neigh_node); // check and create connection between classified and neighbor node
            }
        }
    }
    delete neighborhood;
}


/**
 Complete update of the Sigma-Index. Details on _completeUpdate.
 @param classified The classified population.
 */
template<class U>
void SigmaIndex<U>::completeUpdate(U classified) {
    if(classified==NULL) return;
    SigmaIndexNode<U> *class_node=nodes[classified->getId()]; // find the classified Sigma-Index node containing the supplied population object
    if(class_node->getParents()->size()>0) { // again, as in the update method, check relationships between parent and child population sizes
        vector<SigmaIndexNode<U>*> *v=class_node->getParentsVector();
        for(SigmaIndexNode<U> *pn:*v) {
            if(pn->getId()!=ROOT && pn->getPopulation()->getElements()<class_node->getPopulation()->getElements() &&
               pn->isDirectChild(class_node)) { // if the population size flipped, we reverse the edge direction
                disconnect(pn, class_node);
                connect(class_node, pn);
            }
        }
        delete v;
    }
    _completeUpdate(class_node); // do the complete update of the node
}
template<class U>
void SigmaIndex<U>::_completeUpdate(SigmaIndexNode<U> *class_node) {
    set<string> processed;
    if(class_node->getParents()->size()>0) {
        vector<SigmaIndexNode<U>*> *v=class_node->getParentsVector();
        for(SigmaIndexNode<U> *pn:*v) {
            if(pn->getId()!=ROOT && pn->getPopulation()->mahalanobisDistance(class_node->getPopulation()->getMean())>ntheta) {
                disconnect(pn, class_node);
                processed.insert(pn->getId());
            }
        }
        delete v;
    }
    if(class_node->getChildren()->size()>0) {
        vector<SigmaIndexNode<U>*> *v=class_node->getChildrenVector();
        for(SigmaIndexNode<U> *cn:*v) {
            if(class_node->getPopulation()->mahalanobisDistance(cn->getPopulation()->getMean())>ntheta) {
                disconnect(class_node, cn);
                processed.insert(cn->getId());
            }
        }
        delete v;
    }
    // In the complete update, we iterate through all Sigma-Index nodes and calculate the neighborhood of the classified node
    // This is quite costly, as we perform one sequential-scan classification for each complete update we do
    // Even so, this is quicker than doing sequential-scan for each input data point
    for(pair<string,SigmaIndexNode<U>*> nit:nodes) {
        if(nit.first!=ROOT && *nit.second!=*class_node && processed.find(nit.first)==processed.end() &&
           nit.second->getPopulation()->mahalanobisDistance(class_node->getPopulation()->getMean())<=ntheta) {
            make_connections(nit.second, class_node, true);
        }
    }
}

/**
 Connects two Sigma-Index nodes with edge.
 @param pn The parent node.
 @param cn The child node.
 */
template<class U>
void SigmaIndex<U>::connect(SigmaIndexNode<U> *pn, SigmaIndexNode<U> *cn) {
    if(!pn->isDirectChild(cn) && !cn->isDirectChild(pn)) {
        if(cn->underTheRoot()) cn->removeRootParent(nodes[ROOT]); // If the child node is directly under the ROOT, we remove this edge
        pn->addChild(cn);
    }
}
/**
 Disconnects two Sigma-Index nodes if connected.
 @param pn The parent node.
 @param cn The child node.
 */
template<class U>
void SigmaIndex<U>::disconnect(SigmaIndexNode<U> *pn, SigmaIndexNode<U> *cn) {
    if(pn->isDirectChild(cn)) { // if connected pn->cn
        pn->removeChild(cn); // remove
        if(cn->getParents()->size()==0) cn->moveTo(nodes[ROOT]); // if cn does not have any parent nodes, connect it to the ROOT
    }
}
/**
 Moves a Sigma-Index node from one parent to other.
 @param pn1 The originating parent node.
 @param pn2 The destination parent node.
 @param cn The child node.
 */
template<class U>
void SigmaIndex<U>::move(SigmaIndexNode<U> *pn1, SigmaIndexNode<U> *cn, SigmaIndexNode<U> *pn2) {
    disconnect(pn1, cn);
    connect(pn2, cn);
}

/**
 Internal query method. Algorithm 1 in the Sigma-Index article.
 @param data The input data point that needs to be classified.
 @param node The starting Sigma-Index node. Used for recursion purposes.
 @param results An array that hold the current classification results.
 */
template<class U>
void SigmaIndex<U>::_query(VectorXd *data,SigmaIndexNode<U> *node,SigmaIndexNode<U> *results[]) {
    vector<SigmaIndexNode<U>*> l_neigh1,l_neigh2,*l_neigh=NULL;
    for(pair<string,SigmaIndexNode<U>*> ch_it:*node->getChildren()) { // we iterate through the node children
        if(!ch_it.second->results.visited) { // on each node we log whether it was already visited or not... if not the child node is not visited
            std::chrono::time_point<std::chrono::system_clock> qst=std::chrono::system_clock::now();
            ch_it.second->results.md=ch_it.second->getPopulation()->mahalanobisDistance(data); // calculation for the child node
            std::chrono::time_point<std::chrono::system_clock> qet=std::chrono::system_clock::now();
            qTime+=std::chrono::duration_cast<std::chrono::microseconds>(qet-qst).count();
                // notice that we store the distance on each node - this is what is considered for cache as well
            nodeCounter++; // increment the calculation counter - statistics
            if(!cached) { // if there is no cached data point, cache the one we are classifying now
                cached=true;
                if(cachedData==NULL) cachedData=new VectorXd(*data);
            }
            ch_it.second->results.visited=true; // switch on the visited flag on the child node
            if(ch_it.second->results.md<=ntheta) { // if the data point is in the neighborhood of the child node
                l_neigh1.push_back(ch_it.second); // add the child node to the l1
                results[c1++]=ch_it.second; // add it to the results
                if(ch_it.second->results.md<=theta) ++c2;
                tsCount++; // increment the success counter - statistics
            } else { // if not in the neighborhood
                l_neigh2.push_back(ch_it.second); // add the child node to the l2
                tpCount++; // increment the missed counter - statistics
            }
        }
    }
    for(pair<string,SigmaIndexNode<U>*> pa_it:*node->getParents()) { // iterate through parent nodes
        if(pa_it.first!=ROOT && !pa_it.second->results.visited) { // if the parent node is not ROOT and was not visited yet
            std::chrono::time_point<std::chrono::system_clock> qst=std::chrono::system_clock::now();
            pa_it.second->results.md=pa_it.second->getPopulation()->mahalanobisDistance(data); // calculate the distance for the parent node
            std::chrono::time_point<std::chrono::system_clock> qet=std::chrono::system_clock::now();
            qTime+=std::chrono::duration_cast<std::chrono::microseconds>(qet-qst).count();
            
            nodeCounter++; // increment the calculation counter - statistics
            if(!cached) { // if there is no cached data point, cache the one we are classifying now
                cached=true;
                if(cachedData==NULL) cachedData=new VectorXd(*data);
            }
            if(!switch1) pa_it.second->results.visited=true; // switch on the visited flag on the parent node... we keep the speed here
            if(pa_it.second->results.md<=ntheta) { // for parents we process only those that are in the neighborhood... we trace neighborhood back up
                if(switch1) pa_it.second->results.visited=true; // switch on the visited flag on the parent node
                l_neigh1.push_back(pa_it.second); // add the parent node to l1
                results[c1++]=pa_it.second; // add the parent node to results
                if(pa_it.second->results.md<=theta) ++c2;
                tsCount++; // increment the success counter - statistics
            }
        }
    }

    if(l_neigh1.size()>0) l_neigh=&l_neigh1; // this means we have encountered the neighborhood... we pursuit to explore only these nodes
    else { // if we are not in the neighborhood, then we explore all children
        if(!switch1 && c1>0) return; // if we found the neighborhood somewhere in the upper DAG levels, just return...
        if(switch1 && c2>0) return; // if we found the classified neighborhood somewhere in the upper DAG levels, just return...
        l_neigh=&l_neigh2;
    }
    bool done=false;
    while(!done) { // we iterate until the exit condition... this loop wil execute mostly twice!
        sort(l_neigh->begin(),l_neigh->end(),[=](SigmaIndexNode<U> *a,SigmaIndexNode<U> *b){
            return a->results.md<b->results.md; // we sort the array l by the distance ascending, as we want to explore the closest nodes first
        });
        for(SigmaIndexNode<U> *n_sub:*l_neigh) { // iterate through l
            _query(data,n_sub,results); // the recursive call
            if(!switch1 && (l_neigh1.size()==0 || node->getId()==ROOT) && c1>0) return; // this means we returned back to the ROOT, but we found the neighborhood... stopping here
            if(switch1 && (l_neigh1.size()==0 || node->getId()==ROOT) && c2>0) return; // this means we returned back to the ROOT, but we found the classified neighborhood... stopping here
        }
        if(switch1) { // only if we want to keep precision high
            if(c1>0 && c2==0 && l_neigh==&l_neigh1) l_neigh=&l_neigh2; // so we found the neighborhood, but not classified... and we went through the l1, do it for l2 as well
            else done=true; // otherwise flip the exit flag
        } else {
            if(c1==0 && l_neigh==&l_neigh1) l_neigh=&l_neigh2; // so we did not found the neighborhood... and we went through the l1, do it for l2 as well
            else done=true; // otherwise flip the exit flag
        }
    }
}

/**
 Query method.
 @param data The data point to classify.
 @param exclude The set of populations that needs to be excluded from the query.
 @param starting The starting population.
 @param resetCache Force resetting of the data point cache.
 */
template<class U>
SigmaIndexQueryResults<U> *SigmaIndex<U>::query(VectorXd *data,set<U> *exclude,U starting,
                                        bool resetCache) {
    SigmaIndexNode<U> *results[nodes.size()]; // clean up the results
    c1=0;c2=0; // init the internal counter
    if(resetCache) _resetCache(); // reset the cache
    SigmaIndexNode<U> *start_node=nodes[ROOT]; // find the ROOT
    if(starting!=NULL && nodes.find(starting->getId())!=nodes.end()) start_node=nodes[starting->getId()]; // if there is a starting population supplied from outside
    totalSequential+=(nodes.size()-1); // increase the total sequential count for the total number of nodes
    _query(data, start_node, results); // do the query
    SigmaIndexQueryResults<U> *res=new SigmaIndexQueryResults<U>();
    for(int i=0;i<c1;i++) { // iterate through the results
        SigmaIndexNode<U> *node=results[i];
        U c=node->getPopulation(); // get the population that is in the neighborhood
        if(exclude==NULL || exclude->find(c)==exclude->end()) { // test if we need to exclude this population
            int cz=node->results.getClass(theta,ntheta); // classify whether the population is <=theta or between theta and ntheta
            if(cz==1) res->classified->push_back(pair<U,double>(c,node->results.md)); // add to the classified set
            else if(cz==2) res->neighborhood->push_back(pair<U,double>(c,node->results.md)); // add to the neighborhood set
        }
    }
    sort(res->classified->begin(),res->classified->end(),[=](pair<U,double>& a,pair<U,double>& b) {
        return a.second<b.second; // sort the classified set by the distance ascending
    });
    sort(res->neighborhood->begin(),res->neighborhood->end(),[=](pair<U,double>& a,pair<U,double>& b) {
        return a.second<b.second; // sort the neighborhood set by the distance ascending
    });
    return res;
}

/**
 Removes a Sigma-Index node.
 @param comp The population whose node needs to be removed.
 */
template<class U>
void SigmaIndex<U>::remove(U comp) {
    if(nodes.find(comp->getId())!=nodes.end()) { // if we can find the node that wraps the supplied population
        SigmaIndexNode<U> *node=nodes[comp->getId()],*root_node=nodes[ROOT];
        vector<string> remove1,remove2;
        for(pair<string, SigmaIndexNode<U>*> it1:*node->getChildren()) // iterate through children
            remove1.push_back(it1.first);
        for(string id:remove1) {
            SigmaIndexNode<U> *child=nodes[id];
            node->removeChild(child);
            if(child->getParents()->size()==0) root_node->addChild(child); // if one of the child node looses all parents, add it to the ROOT node
        }
        for(pair<string, SigmaIndexNode<U>*> it2:*node->getParents()) // iterate through parents
            remove2.push_back(it2.first);
        for(string id:remove2)
            nodes[id]->removeChild(node);
        nodes.erase(node->getId()); // remove the node from the map
        delete node; // delete the Sigma-Index node object
    }
}

/**
 Prints the Sigma-Index. Basic orientative, debugging text printout.
 @param node_id The node identifier.
 @param ostr The output stream for printout, e.g., cout.
 @param visited The set of already visited nodes.
 */
template<class U>
void SigmaIndex<U>::print(string node_id,ostream &ostr,set<string> *visited) {
    if(!visited) visited=new set<string>();
    if(visited->find(node_id)!=visited->end()) return; // mark visited nodes
    visited->insert(node_id);
    SigmaIndexNode<U> *node=nodes[node_id]; // find the Sigma-Index node having the supplied identifier
    ostr << (node_id==ROOT ? "++++++++++++++++++++++" : "----------------------") << endl;
    ostr << "Node:" << node_id << " elements=" << (node_id!=ROOT ? node->getPopulation()->getElements() : 0) << endl;
    if(node->getChildren()->size()>0) { // iterate through his children
        ostr << "Children:";
        for(pair<string,SigmaIndexNode<U>*> it:*node->getChildren())
            ostr << it.first << "(" << it.second->getPopulation()->getElements() << ") ";
        ostr << endl;
        for(pair<string,SigmaIndexNode<U>*> it:*node->getChildren())
            print(it.first,ostr,visited);
    }
}

/**
 Calculates and returns the neighborhood multiplier, i.e., ntheta/theta
 @return The multiplier.
 */
template<class U>
int SigmaIndex<U>::getNeighborhoodMultiplier() {
    return (int)ntheta/theta;
}

/**
 Clones the Sigma-Index.
 @return The cloned Sigma-Index.
 */
template<class U>
SigmaIndex<U> *SigmaIndex<U>::clone() {
    SigmaIndex *nst=new SigmaIndex(theta,ntheta,switch1); // instantiate the cloned object
    nst->cached=this->cached; // clone the cached flag
    if(this->cachedData!=NULL) nst->cachedData=new VectorXd(*this->cachedData); // clone the cached data point
    for(pair<string,SigmaIndexNode<U>*> it:nodes) { // iterate through node map
        if(it.first!=ROOT) { // if not the ROOT node
            SigmaIndexNode<U> *nn=new SigmaIndexNode<U>(it.second->getPopulation()); // clone the Sigma-Index node, reroute the population pointer
            nn->results=it.second->results.clone(); // clone the cached data - visited flags and distances
            nst->nodes[it.second->getPopulation()->getId()]=nn; // update the node map ine cloned Sigma-Index
        }
    }
    for(pair<string,SigmaIndexNode<U>*> it:nodes) { // iterate through nodes in the local map
        SigmaIndexNode<U> *nn=nst->nodes[it.first]; // find the same node in the cloned Sigma-Index
        for(pair<string,SigmaIndexNode<U>*> itp:*it.second->getParents()) // clone edges to parents
            (*nn->getParents())[itp.first]=nst->nodes[itp.first];
        for(pair<string,SigmaIndexNode<U>*> itp:*it.second->getChildren()) // clone edges to children
            (*nn->getChildren())[itp.first]=nst->nodes[itp.first];
    }
    return nst;
}

/**
 Creates new Sigma-Index node. Usually for a new population. Or an outlier.
 @param comp The new population.
 @param resetCache Flag that indicated whether to force reset the previous cache data.
 */
template<class U>
void SigmaIndex<U>::create(U comp,bool resetCache) {
    if(nodes.find(comp->getId())==nodes.end()) { // check the population identifier in the current nodes
        SigmaIndexNode<U> *nn=new SigmaIndexNode<U>(comp); // create new Sigma-Index node
        nodes[comp->getId()]=nn; // add it to the map
        nodes[ROOT]->addChild(nn); // add it under the ROOT node... this is a default edge
        if(!cached || resetCache) { // if we do not want to use the cache
            if(cached) _resetCache(); // reset cached data
            for(pair<string,SigmaIndexNode<U>*> it:nodes) { // do the full update... this is quite costly
                if(it.first!=comp->getId() && it.first!=ROOT)
                    make_connections(nn, it.second);
            }
        } else { // if we want to use the previously cached data...
            // this usually happens when we performed a query priori to this create
            // and we did not classify the input data point... but now we want to reuse neighborhood calculations
            // to ease creation... this is similar to the incremental update
            for(pair<string,SigmaIndexNode<U>*> it:nodes) { // iterate through the nodes
                if(it.second->results.visited && it.second->results.md<=ntheta) // if the cached distance indicates that this node is in the neighborhood
                    make_connections(nn, it.second, true); // connect it
            }
            vector<SigmaIndexNode<U>*> unprocessed;
            for(pair<string,SigmaIndexNode<U>*> it:*nodes[ROOT]->getChildren()) { // iterate through top nodes, those directly under the ROOT
                if(it.second->results.visited && it.second->getChildren()->size()>0 &&
                   !it.second->getChildren()->begin()->second->results.visited)
                    // the top node is usually processed... but we check the second row of nodes
                    // those that are unvisited, we want to process them
                    unprocessed.push_back(it.second); // we store top-nodes that we want to process
            }
            sort(unprocessed.begin(),unprocessed.end(),[=](SigmaIndexNode<U> *a,SigmaIndexNode<U> *b) {
                return a->results.md<b->results.md; // sort the unprocessed nodes by distance ascending
            });
            for(SigmaIndexNode<U> *cn1:unprocessed) { // iterate through unprocessed nodes
                SigmaIndexNode<U> *results[nodes.size()];
                c1=0;c2=0;
                _query(cachedData, cn1, results);   // do the query only for one unprocessed top node - notice that we do not start from the ROOT here
                if(!switch1 && c1==0) return; // exit early only if we do not want to keep the precision
                // but we query only a sub-DAG
                // we expect that we find any remaining neighborhood in the first few iterations
                // until the query did not find any neighborhood nodes ... then we exit
                for(int i=0;i<c1;i++) { // if we found some results in the query
                    SigmaIndexNode<U> *cn2=results[i];
                    make_connections(nn, cn2, true); // connect to the new node
                }
            }
        }
    }
}

/**
 Resets the statistic counters.
 */
template<class U>
void SigmaIndex<U>::resetStatistics() {
    this->nodeCounter=0;
    this->tpCount=0;
    this->tsCount=0;
    this->totalSequential=0;
    this->qTime=0;
}

/**
 Returns the statistic structure.
 @return The statistic structure.
 */
template<class U>
SigmaIndexStatistics *SigmaIndex<U>::getStatistics() {
    return new SigmaIndexStatistics(nodeCounter,tpCount,tsCount,totalSequential,qTime);
}

/**
 Resets the cached data.
 */
template<class U>
void SigmaIndex<U>::_resetCache() {
    cached=false; // switch off the cached flag
    if(cachedData!=NULL) delete cachedData; // delete the cached data point
    cachedData=NULL;
    for(pair<string,SigmaIndexNode<U>*> it:nodes) it.second->results.reset(); // iterate through nodes and reset the cached data
}

/**
 Calculates the complete histogram for the Sigma-Index.
 @return The map 0-100 containing node counts for each computation cost reduction.
 */
template<class U>
unordered_map<int, long> *SigmaIndex<U>::calculateHistogram() {
    unordered_map<int, long> *hist=new unordered_map<int, long>();
    for(pair<string, SigmaIndexNode<U>*> it:nodes) { // iterate through all nodes
        if(it.first!=ROOT) { // if not the ROOT node
            resetStatistics(); // reset the statistic counters for each node
            SigmaIndexQueryResults<U> *res=query(it.second->getPopulation()->getMean()); // do the query for the population centroid
            SigmaIndexStatistics *stats=getStatistics(); // retrieve statistics
            int bin=ceil((stats->R<0 ? 0 : stats->R)*100); // calculate CCR as percentage
            if(hist->find(bin)!=hist->end()) (*hist)[bin]=(*hist)[bin]+it.second->getPopulation()->getElements(); // multiply this CCR with population size and add to the CCR bin
            else (*hist)[bin]=it.second->getPopulation()->getElements();
            delete stats;delete res;
        }
    }
    return hist;
}

/**
 Creates connection between Sigma-Index nodes
 @param c The node 1.
 @param n The node 2.
 @param force The force flag.
 */
template<class U>
void SigmaIndex<U>::make_connections(SigmaIndexNode<U> *c,SigmaIndexNode<U> *n,bool force) {
    if(*c==*n) return; // cannot create loops on nodes
    if(n->getPopulation()->getElements()<c->getPopulation()->getElements()) { // if n is smaller then c, n should be the child
        if(n->isDirectChild(c)) disconnect(n, c); // if we already have an edge but in the opposite direction, remove it
        if(!c->isDirectChild(n) && (force || c->getPopulation()->mahalanobisDistance(n->getPopulation()->getMean())<=ntheta ||
                                    n->getPopulation()->mahalanobisDistance(c->getPopulation()->getMean())<=ntheta)) connect(c, n);
            // check if there is no edge c->n and n centroid is in the neighborhood of c, make the connection c->n
    }
    if(n->getPopulation()->getElements()>c->getPopulation()->getElements()) { // if c is smaller then n, c should be the child
        if(c->isDirectChild(n)) disconnect(c, n); // if we already have an edge but in the opposite direction, remove it
        if(!n->isDirectChild(c) && (force || n->getPopulation()->mahalanobisDistance(c->getPopulation()->getMean())<=ntheta ||
                                    c->getPopulation()->mahalanobisDistance(n->getPopulation()->getMean())<=ntheta)) connect(n, c);
            // check if there is no edge n->c and c centroid is in the neighborhood of n, make the connection n->c
    }
    if(n->getPopulation()->getElements()==c->getPopulation()->getElements() && !n->isDirectChild(c) && !c->isDirectChild(n)) {
        // if the nodes are equal, we can make the connection but only:
        // - if there is no connection between these nodes (at all)
        // - c centroid is in the neighborhood of n, then we create n->c
        if(force || n->getPopulation()->mahalanobisDistance(c->getPopulation()->getMean())<=ntheta ||
           c->getPopulation()->mahalanobisDistance(n->getPopulation()->getMean())<=ntheta) {
            connect(n, c);
        }
    }
}

/**
 Returns the ROOT node
 @return The ROOT node.
 */
template<class U>
SigmaIndexNode<U> *SigmaIndex<U>::getRoot() {
    return nodes[ROOT];
}
