#ifndef SHC_Container_hpp
#define SHC_Container_hpp

#include <string>
#include <unordered_map>
#include <mutex>
#include <set>
using namespace std;

class SHC_Containable;
class SHC_Component;
class SHC_Containable_Set {
private:
    unordered_map<string, SHC_Containable *> _set;
public:
    SHC_Containable_Set()=default;
    ~SHC_Containable_Set();
    void addContainer(SHC_Containable *container);
    void addComponent(SHC_Component *comp);
    void removeContainer(string *id);
    SHC_Containable *fetchContainer(string *id);
    unordered_map<string, SHC_Containable *> *getAllContainers();
};

class SHC_Containable {
private:
    string id;
protected:
    mutex container_m;
    SHC_Containable *parent=NULL;
    unordered_map<string,SHC_Containable*> children;
    SHC_Containable_Set *set=NULL;
    std::set<string> trace;
    void forcesetId(string id);
public:
    SHC_Containable();
    SHC_Containable(SHC_Containable_Set *set);
    SHC_Containable(SHC_Containable_Set *set, SHC_Containable *initChild);
    virtual ~SHC_Containable();
    string getId();
    void setId(string id);
    SHC_Containable* getParent();
    void setParent(SHC_Containable* parent);
    SHC_Containable* findTop();
    void addChild(SHC_Containable* child);
    void removeChild(SHC_Containable* child);
    unordered_map<string,SHC_Containable*> *getChildren();
    void detachFromParent();
    void attachToParent(SHC_Containable *parent);
    unordered_map<string,SHC_Containable*> fetchChildComponents();
    double calculateSeparation(SHC_Containable *cont, double theta);
    SHC_Containable *fetchTopContainer();
    bool hasChildren();
    void addTrace(SHC_Containable *previous);
    void addTrace(pair<string, std::set<string>> *prev_pair);
    void addTrace(std::set<string> *prev_log);
    bool checkTrace(string id);
    std::set<string> *getTrace();
};

#endif /* SHC_Container_hpp */
