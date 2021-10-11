#ifndef SHC_ComponentConnection_hpp
#define SHC_ComponentConnection_hpp

#include "SHC_Component.hpp"
#include <unordered_map>
#include <set>
using namespace std;

template<typename T1, typename T2> struct SHC_ComponentConnectionId
{
    T1 x;
    T2 y;
    SHC_ComponentConnectionId(T1 x, T2 y) {
        this->x = x;
        this->y = y;
    }
    bool operator==(const SHC_ComponentConnectionId &cc) const {
        return (x==cc.x && y==cc.y) || (x==cc.y && y==cc.x);
    }
};

struct SHC_ComponentConnection_hash {
    template <class T1, class T2>
    size_t operator() (const SHC_ComponentConnectionId<T1, T2> &cc) const {
        return hash<T1>()(cc.x)^hash<T2>()(cc.y);
    }
};

class SHC_ComponentConnection {
private:
    string id;
    SHC_Component *c1=NULL,*c2=NULL;
    long points=1;
public:
    SHC_ComponentConnection(SHC_Component *c1, SHC_Component *c2);
    void addPoint();
    bool operator==(const SHC_ComponentConnection &cc) const;
    SHC_Component *getConnectedComponent(SHC_Component *c);
    SHC_Component *getComponent1();
    SHC_Component *getComponent2();
    long getPoints();
    void setPoints(long v);
    void addPoints(long v);
};

class SHC_ComponentConnection_Set {
private:
    unordered_map<SHC_ComponentConnectionId<string, string>, SHC_ComponentConnection *, SHC_ComponentConnection_hash> _set;
    unordered_map<string, set<SHC_Component *> *> _links;
    void _getConnectedSet(SHC_Component *_from_c,set<SHC_Component *> *_in_set);
    vector<pair<long,set<SHC_Component *> *>> *calculatePartitions(set<SHC_Component *> *_in_set);
    bool _partition2(set<SHC_Component *> *_is1, set<SHC_Component *> *_is2);
public:
    SHC_ComponentConnection_Set()=default;
    ~SHC_ComponentConnection_Set();
    SHC_ComponentConnection *connect(SHC_Component *c1, SHC_Component *c2, bool add_point=true);
    bool isConnected(SHC_Component *c1, SHC_Component *c2, bool directly=false);
    set<SHC_Component *> *getConnectedComponents(SHC_Component *c);
    SHC_Component *getMaxConnectedComponents(SHC_Component *c);
    vector<pair<long,set<SHC_Component *> *>> *removeComponent(SHC_Component *c);
    set<SHC_ComponentConnection *> *getConnections();
};

#endif /* SHC_ComponentConnection_hpp */
