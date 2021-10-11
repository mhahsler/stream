#ifndef SHC_Decay_hpp
#define SHC_Decay_hpp

#include <stdio.h>
#include <Eigen/Dense>
#include <algorithm>
#include "SHC_Component.hpp"
using namespace Eigen;
using namespace std;

class SHC_Component;
class SHC;
class SHC_Decay {
public:
    SHC_Decay();
    virtual void reset();
    virtual ~SHC_Decay()=default;
};

class SHC_Count_Decay : public SHC_Decay {
private:
    long ceil_count,counter;
public:
    SHC_Count_Decay(long ceil_count);
    ~SHC_Count_Decay();
    void reset();
    void reset(long ceil_count);
    bool ping();
    SHC_Count_Decay *clone();
    long getCeilingCount();
    long getCurrentPos();
};

#endif /* SHC_Decay_hpp */
