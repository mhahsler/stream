#include "SHC_Decay.hpp"

SHC_Decay::SHC_Decay() {
}
void SHC_Decay::reset() {}

SHC_Count_Decay::SHC_Count_Decay(long ceil_count):SHC_Decay() {
    this->ceil_count=ceil_count;
    this->counter=ceil_count;
}

SHC_Count_Decay::~SHC_Count_Decay()=default;

void SHC_Count_Decay::reset() {
    counter=ceil_count;
}
void SHC_Count_Decay::reset(long ceil_count) {
    this->ceil_count=ceil_count;
    this->counter=ceil_count;
}
bool SHC_Count_Decay::ping() {
    counter--;
    return counter<0;
}

SHC_Count_Decay *SHC_Count_Decay::clone() {
    SHC_Count_Decay *cc=new SHC_Count_Decay(ceil_count);
    cc->counter=this->counter;
    return cc;
}

long SHC_Count_Decay::getCeilingCount() {
    return ceil_count;
}

long SHC_Count_Decay::getCurrentPos() {
    return counter;
}
