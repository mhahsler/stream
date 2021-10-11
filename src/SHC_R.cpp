#include <RcppEigen.h>
#include <SHC>
#include <SHC_Component>
using namespace Rcpp;
using namespace Eigen;

class SHC_R {
private:
  SHC *shc_classifier=NULL;
  unordered_map<string,int> clus_map,comp_map;
  unordered_map<int,string> bkclus_map,bkcomp_map;
  int clus_ind=1,comp_ind=1;
  int _getClusterMapping(string s_clusId) {
    if(clus_map.find(s_clusId)==clus_map.end()) {
      bkclus_map[clus_ind]=s_clusId;
      clus_map[s_clusId]=clus_ind++;
    }
    return clus_map[s_clusId];
  }
  int _getComponentMapping(string s_compId) {
    if(comp_map.find(s_compId)==comp_map.end()) {
      bkcomp_map[comp_ind]=s_compId;
      comp_map[s_compId]=comp_ind++;
    }
    return comp_map[s_compId];
  }
  string *_getBkClusterMapping(int cluster) {
    if(bkclus_map.find(cluster)!=bkclus_map.end()) return new string(bkclus_map[cluster]);
    else return NULL;
  }
  string *_getBkComponentMapping(int component) {
    if(bkcomp_map.find(component)!=bkcomp_map.end()) return new string(bkcomp_map[component]);
    else return NULL;
  }
public:
  SHC_R(int dimensions,int aggloType=0,int driftType=0,int decayPace=10,int sha_agglo_theta=1) {
    AgglomerationType at=NormalAgglomeration;
    switch(aggloType) {
      case 1: at=AggresiveAgglomeration;break;
      case 2: at=RelaxedAgglomeration;break;
      default: at=NormalAgglomeration;break;
    }
    DriftType dt=NormalDrift;
    switch(driftType) {
      case 1: dt=FastDrift;break;
      case 2: dt=SlowDrift;break;
      case 3: dt=NoDrift;break;
      case 4: dt=UltraFastDrift;break;
      default: dt=NormalDrift;break;
    }
    shc_classifier=new SHC(dimensions,at,dt,decayPace);
    if(sha_agglo_theta>1) shc_classifier->setSharedAgglomerationThreshold(sha_agglo_theta);
  }
  SHC_R(List params) {
    double theta=params["theta"];
    bool par=params["parallelize"];
    bool pSA=params["performSharedAgglomeration"];
    VectorXd vv=as<VectorXd>(params["virtualVariance"]);
    int acount=params["agglo_count"];
    double cbV=params["cbVarianceLimit"];
    int cbN=params["cbNLimit"];
    double drCSR=params["driftRemoveCompSizeRatio"];
    double dCSR=params["driftCheckingSizeRatio"];
    double dMDTR=params["driftMovementMDThetaRatio"];
    int dp=params["decayPace"];
    double compFormingMinVVRatio=params["componentFormingMinVVRatio"];
    double compBlockingLimitVVRatio=params["componentBlockingLimitVVRatio"];
    shc_classifier=new SHC(theta,par,pSA,&vv,acount,cbV,cbN,(float)drCSR,(float)dCSR,
                           (float)dMDTR,dp,(float)compFormingMinVVRatio,
                           (float)compBlockingLimitVVRatio);
    int sha_at=params["sharedAgglomerationThreshold"];
    if(sha_at>1) shc_classifier->setSharedAgglomerationThreshold(sha_at);
  }
  SHC_R(SHC_R *ancestor_shc) {
    shc_classifier=ancestor_shc->shc_classifier->clone();
  }
  ~SHC_R() {
    if(shc_classifier!=NULL) delete shc_classifier;
  }
  DataFrame processForR(DataFrame elements,bool classifyOnly) {
    Nullable<StringVector> l=elements.attr("cluster");
    NumericMatrix nm=Rcpp::internal::convert_using_rfunction(elements, "as.matrix");
    MatrixXd m=as<MatrixXd>(nm);
    pair<shared_ptr<vector<shared_ptr<ClassificationResult>>>,shared_ptr<DeltaLogger>> res_tup=shc_classifier->process(&m,false,classifyOnly);
    shared_ptr<vector<shared_ptr<ClassificationResult>>> res=res_tup.first;
    //if(!classifyOnly) shc_classifier->pseudoOffline(true);
    StringVector clus,comps;
    IntegerVector clus_m,comps_m;
    LogicalVector outliers;
    for(int i=0;i<res->size();i++) {
      shared_ptr<ClassificationResult> cres=res->at(i);
      if(cres->cluster_id!=NULL) {
        clus.push_back(*cres->cluster_id);
        clus_m.push_back(_getClusterMapping(*cres->cluster_id));
      } else {
        clus.push_back(NA_STRING);
        clus_m.push_back(0);
      }
      if(cres->component_id!=NULL) {
        comps.push_back(*cres->component_id);
        comps_m.push_back(_getComponentMapping(*cres->component_id));
      } else {
        comps.push_back(NA_STRING);
        comps_m.push_back(0);
      }
      outliers.push_back(cres->outlier);
    }
    if(l.isNull()) return DataFrame::create(Named("cluster_id")=clus,Named("component_id")=comps,Named("assigned_cluster")=clus_m,Named("assigned_comp")=comps_m,Named("outlier")=outliers,Named("stringsAsFactors")=false);
    else return DataFrame::create(Named("cluster_id")=clus,Named("component_id")=comps,Named("assigned_cluster")=clus_m,Named("assigned_comp")=comps_m,Named("outlier")=outliers,Named(".clazz")=StringVector(l),Named("stringsAsFactors")=false);
  }
  StringVector getClusters(bool clusters,bool outliers) {
    set<string> *clusts=shc_classifier->getTopContainers(clusters,outliers);
    StringVector res;
    for(string it:*clusts) res.push_back(it);
    delete clusts;
    return res;
  }
  StringVector getComponents(String clusterId) {
    string s=(string)clusterId;
    vector<SHC_Component*> *comps=shc_classifier->getComponents(&s);
    StringVector res;
    for(SHC_Component *c:*comps) res.push_back(c->getId());
    delete comps;
    return res;
  }
  StringVector getAllComponents() {
    unordered_map<string, SHC_Component*> *comps=shc_classifier->getComponents();
    StringVector res;
    for(pair<string, SHC_Component*> it:*comps) res.push_back(it.first);
    return res;
  }
  List getClusterContours(String clusterId) {
    string s=(string)clusterId;
    vector<SHC_Component_Details *> *clus_det=shc_classifier->getClassificationDetails(&s);
    List res=List::create();
    for(SHC_Component_Details *comp_det:*clus_det) {
      res[comp_det->parent->getId()]=wrap(*comp_det->single);
      delete comp_det;
    }
    delete clus_det;
    return res;
  }
  void cleanOutliers() {
    shc_classifier->cleanOutliers();
  }
  NumericMatrix getOutlierPositions() {
    vector<SHC_Component*> *outs=shc_classifier->getOutliers();
    NumericMatrix m(outs->size(),shc_classifier->getVirtualVariance()->size());
    int i=0;
    for(SHC_Component *c:*outs) {
      NumericVector mu(wrap(*c->getMean()));
      m(i++,_)=mu;
    }
    delete outs;
    return m;
  }
  List getComponentDetails(String componentId) {
    string s=(string)componentId;
    SHC_Component *comp=shc_classifier->getComponent(&s);
    if(comp==NULL) return R_NilValue;
    List res=List::create();
    res["component_id"]=componentId;
    res["cluster_id"]=comp->getParent()->getId();
    res["dimensions"]=comp->getDimensions();
    res["is_outlier"]=comp->isOutlier();
    res["elements"]=comp->getElements();
    res["mean"]=wrap(*comp->getMean());
    MatrixXd *cov=comp->getCovariance();
    res["covariance"]=wrap(*cov);
    delete cov;
    res["is_cov_inverted"]=comp->isCovarianceInverted();
    res["has_baseline"]=comp->hasBaseline();
    res["is_obsolete"]=comp->isObsolete();
    if(comp->getDimensions()==2) {
      SHC_Component_Details *cd1=comp->calculateDetails(shc_classifier->getTheta());
      res["component_contour"]=wrap(*cd1->single);
      delete cd1;
      if(comp->hasBaseline()) {
        SHC_Component *bline=comp->getBaseline();
        SHC_Component_Details *cd2=bline->calculateDetails(shc_classifier->getTheta());
        res["component_baseline_contour"]=wrap(*cd2->single);
        delete cd2;
      }
    }
    return res;
  }
  double getTheta() {
    return shc_classifier->getTheta();
  }
  NumericVector getVirtualVariance() {
    return wrap(*shc_classifier->getVirtualVariance());
  }
  bool isTraceable(String fromComponentId,String toComponentId) {
    string fcid=(string)fromComponentId;
    set<string> *tl=shc_classifier->traceComponentId(&fcid);
    bool res=tl->find((string)toComponentId)!=tl->end();
    if(!res) {
      Rcout << "Trace for " << fcid << ":";
      for(string str:*tl) Rcout << str << " ";
      Rcout << endl;
    }
    delete tl;
    return res;
  }
  StringVector getTrace(String fromComponentId) {
    string fcid=(string)fromComponentId;
    set<string> *tl=shc_classifier->traceComponentId(&fcid);
    StringVector res;
    for(string str:*tl) res.push_back(str);
    delete tl;
    return res;
  }
  double getClusterWeight(String clusterId) {
    long telms=shc_classifier->getTotalElements(true,true);
    string clusId=(string)clusterId;
    vector<SHC_Component*> *comps=shc_classifier->getComponents(&clusId);
    long tclus=0;
    for(SHC_Component *comp:*comps) tclus+=comp->getElements();
    delete comps;
    return telms>0 ? tclus/telms : (double)0;
  }
  double getComponentWeight(String componentId) {
    long telms=shc_classifier->getTotalElements(true,true);
    string compId=(string)componentId;
    SHC_Component *comp=shc_classifier->getComponent(&compId);
    long tcomp=comp->getElements();
    return telms>0 ? tcomp/telms : (double)0;
  }
  int getClusterMapping(String clusId) {
    return _getClusterMapping((string)clusId);
  }
  int getComponentMapping(String compId) {
    return _getComponentMapping((string)compId);
  }
  IntegerVector stream_MicroToMacro(IntegerVector micro_vec) {
    IntegerVector res;
    for(int it:micro_vec) {
      string *bk=_getBkComponentMapping(it);
      if(bk!=NULL) {
        SHC_Component *comp=shc_classifier->getComponent(bk);
        if(comp!=NULL && comp->getParent()!=NULL)
          res.push_back(_getClusterMapping(comp->getParent()->getId()));
        else {
          set<string> *new_comps=shc_classifier->traceComponentId(bk);
          if(new_comps->size()==0) res.push_back(0);
          else {
            bool done=false;
            for(string it2:*new_comps) {
              SHC_Component *nc=shc_classifier->getComponent(&it2);
              if(nc!=NULL) {
                int v2=_getClusterMapping(nc->getParent()->getId());
                if(v2>0 && !done) {
                  res.push_back(v2);
                  done=true;
                }
              }
            }
            if(!done) res.push_back(0);
          }
          delete new_comps;
        }
        delete bk;
      } else res.push_back(0);
    }
    return res;
  }
  IntegerVector stream_MicroToMicro(int micro) {
    IntegerVector res;
    if(micro==0) return res;
    string *bk=_getBkComponentMapping(micro);
    if(bk!=NULL) {
      SHC_Component *comp=shc_classifier->getComponent(bk);
      if(comp==NULL) {
        set<string> *ncomps=shc_classifier->traceComponentId(bk);
        for(string it2:*ncomps) {
          int v2=_getComponentMapping(it2);
          if(v2>0) res.push_back(v2);
        }
        delete ncomps;
      } else {
        int v2=_getComponentMapping(comp->getId());
        if(v2>0) res.push_back(v2);
      }
      delete bk;
    }
    return res;
  }
  List getStats() {
    pair<long,long> stats=shc_classifier->getStatistic();
    List res=List::create();
    res["components"]=stats.first;
    res["outliers"]=stats.second;
    return res;
  }
  void useSigmaIndex(int neigh_multiplier, bool precision_switch) {
    shc_classifier->useSigmaIndex(neigh_multiplier, precision_switch);
  }
  void setPseudoOfflineCounter(int agglo_count) {
    shc_classifier->setAgglomerationCount(agglo_count);
  }
  List getTimes() {
    vector<double> times=shc_classifier->getTimes();    
    List res=List::create();
    res["queryTime"]=times[0];
    res["updateTime"]=times[1];
    res["processTime"]=times[2];
    return res;
  }
  long getNodeCounter() {
    return shc_classifier->getNodeCounter();
  }
  double getSigmaIndexR() {
    SigmaIndex<SHC_Component*> *st=shc_classifier->getSigmaIndex();
    if(st==NULL) return (double)0;
    SigmaIndexStatistics *st_stats=st->getStatistics();
    double R=st_stats->R;
    delete st_stats;
    return R;
  }
  NumericMatrix getSigmaIndexHistogram() {
    SigmaIndex<SHC_Component*> *st=shc_classifier->getSigmaIndex();
    if(st==NULL) return NumericMatrix(0,0);
    unordered_map<int,long> *hist=st->calculateHistogram();
    IntegerVector xv,yv;
    for(int it=0;it<=100;it++) {
      xv.push_back(it);
      if(hist->find(it)!=hist->end()) yv.push_back((*hist)[it]);
      else yv.push_back(0);
    }
    NumericMatrix res(1,101,yv.begin());
    colnames(res)=xv;
    delete hist;
    return res;
  }
  bool recheckOutlier(String id) {
    string s_id=(string)id;
    return shc_classifier->recheckOutlier(&s_id);
  }
  SHC_R *cloneSHC() {
    return new SHC_R(this);
  }
  void clearEigenMPSupport() {
    shc_classifier->clearEigenMPSupport();
  }
};