#include "SHC_Component.hpp"
#include "SHC.hpp"
#include <string>
#include <iostream>
#include <set>
#include <functional>
#include <Eigen/Dense>
using namespace std;
using namespace Eigen;

SHC_Component_Exception::SHC_Component_Exception(const char *msg) {
    this->msg=msg;
}
const char *SHC_Component_Exception::what() const throw() {
    return this->msg;
}

SHC_Component::SHC_Component(SHC_Containable_Set *set,VectorXd *newElement,VectorXd *virtualVariance,double cbVarianceLimit,long cbNLimit,
                             float driftCheckingSizeRatio,float driftMovementMDThetaRatio,int decayPace,float componentFormingMinVVRatio,
                             float componentBlockingLimitVVRatio) : SHC_Containable(set) {
    if(set!=NULL) set->addComponent(this);
    if(newElement==NULL) throw SHC_Component_Exception("New element must be defined");
    this->dim=newElement->size();
    if(virtualVariance!=NULL && virtualVariance->size()!=dim) throw SHC_Component_Exception("Virtual variance vector does not have the same dimension as the new element");
    this->cov=new MatrixXd(dim,dim);
    this->cov->setZero();
    this->mean=new VectorXd(*newElement);
    MatrixXd tmp(dim,dim);
    tmp.setZero();
    if(virtualVariance==NULL)
        for(int i=0;i<dim;i++) tmp(i,i)=DEFAULT_VIRTUAL_VARIANCE;
    else {
        for(int i=0;i<dim;i++) tmp(i,i)=(*virtualVariance)(i);
        this->vv_value=(*virtualVariance)(0);
    }
    this->vv=new MatrixXd(tmp.inverse());
    this->elements=1;
    this->cbVarianceLimit=cbVarianceLimit;
    this->cbNLimit=cbNLimit;
    this->driftCheckingSizeRatio=driftCheckingSizeRatio;
    this->driftMovementMDThetaRatio=driftMovementMDThetaRatio;
    this->componentFormingMinVVRatio=componentFormingMinVVRatio;
    this->componentBlockingLimitVVRatio=componentBlockingLimitVVRatio;
    this->decay_pace=decayPace;
    if(decay_pace>0 && cbNLimit>0) this->decay_handler=new SHC_Count_Decay(20*decay_pace*cbNLimit);
}

SHC_Component::~SHC_Component() {
    if(isRedirected()) redirect_to->removeFromRedirectedComponent(this);
    for(auto redir_from:redirected_from) {
        if(isRedirected()) redir_from.second->redirectComponent(redirect_to);
        else redir_from.second->redirectComponent(NULL);
    }
    delete mean;
    delete vv;
    delete cov;
    if(baseline!=NULL) delete baseline;
    if(front_connection_mapping!=NULL) {
        for(auto it:*front_connection_mapping) delete it.second;
        delete front_connection_mapping;
    }
    if(front_resolver!=NULL) delete front_resolver;
    if(decay_handler!=NULL) delete decay_handler;
}

bool SHC_Component::isCovInvertible() {
    if(inverted) throw SHC_Component_Exception("Covariance matrix already inverted");
    FullPivLU<MatrixXd> lu(*cov);
    return lu.isInvertible();
}

bool SHC_Component::addNewElement(VectorXd *newElement,SHC *parent_resolver,vector<pair<SHC_Component*,double>> *classified_map) {
    if(newElement==NULL) throw SHC_Component_Exception("New element must be defined");
    if(newElement->size()!=dim) {
        throw SHC_Component_Exception("New element does not have the same dimension as the component");
    }
    if(redirect_to!=NULL)
        return redirect_to->addNewElement(newElement,parent_resolver,classified_map);
    if(obsolete) return false;
    if(outlier && componentFormingMinVVRatio>0) {
        VectorXd v1=*newElement-*mean;
        MatrixXd B=v1*v1.transpose()/2;
        auto Barr=abs(B.array());
        double lim1=componentFormingMinVVRatio*vv_value;
        bool cont=false;
        if((Barr>=lim1).any()) cont=true;
        if(!cont) return false;
        double lim2=componentBlockingLimitVVRatio*vv_value;
        if(componentBlockingLimitVVRatio>0 && (Barr>lim2).any())
            return false;
    }
    if(agglo_out_blocker>0) agglo_out_blocker--;
    if(decay_handler) decay_handler->reset(elements*decay_pace<cbNLimit*decay_pace ? decay_pace*cbNLimit : decay_pace*elements);
    if(blocked) return false;
    // calculate new mean
    parent_resolver->incDeltaElementsInDeltaLog(this);
    VectorXd *old_mean=mean;
    VectorXd t1=(*newElement-*old_mean)/(elements+1);
    mean=new VectorXd(*old_mean+t1);
    // calculate new covariance
    MatrixXd *old_cov=cov;
    if(!inverted) {
        MatrixXd B=(((*newElement-*old_mean)*(*newElement-*mean).transpose())-*old_cov)/(elements+1);
        cov=new MatrixXd(*old_cov+B);
        if(isCovInvertible()) {
            MatrixXd *tcov=cov;
            cov=new MatrixXd(tcov->inverse());
            inverted=true;
            delete tcov;
        }
    } else {
        MatrixXd u=(*newElement-*old_mean)/sqrt(elements);
        MatrixXd vt=((*newElement-*mean)/sqrt(elements)).transpose();
        MatrixXd v1=*old_cov*u*vt**old_cov;
        MatrixXd v2=vt**old_cov*u;
        MatrixXd res=*old_cov-v1/((double)1+v2(0,0));
        double X=(double)elements/((double)elements+(double)1);
        cov=new MatrixXd(res/X);
    }
    delete old_mean;delete old_cov;
    outlier=false;
    elements++;
    if(elements==2) removeFromNeighborhood(parent_resolver);
    MatrixXd *tmp_cov=getCovariance();
    if(!blocked && inverted && componentBlockingLimitVVRatio>0 && elements>10) { // chameleon=10
        auto x=abs((*tmp_cov).array());
        double lim=componentBlockingLimitVVRatio*vv_value;
        if((x>lim).any()) {
            blocked=true;
            cleanRedirection();
            neighborhood.erase(neighborhood.begin(),neighborhood.end());
            if(isRedirected()) {
                redirect_to->removeFromRedirectedComponent(this);
                redirect_to=NULL;
            }
        }
    }
    if((cbVarianceLimit>0 || cbNLimit>0) && inverted && !baseline && ((tmp_cov->array()>cbVarianceLimit).any() || elements>cbNLimit)) {
        baseline=clone();
        front_resolver=new SHC(dim,AggresiveAgglomeration,NoDrift,true);
        front_resolver->switchDecay(false);
        front_connection_mapping=new unordered_map<string, std::set<string>*>();
    }
    delete tmp_cov;
    if(parent_resolver->getTheta()>0 && !obsolete && baseline) {
        if(!obsolete && front_resolver!=NULL) {
            shared_ptr<ClassificationResult> class_res=front_resolver->process(newElement);
            if(classified_map!=NULL) {
                if(front_connection_mapping->find(*class_res->component_id)==front_connection_mapping->end())
                    (*front_connection_mapping)[*class_res->component_id]=new std::set<string>();
                for(pair<SHC_Component*,double> it:*classified_map)
                    (*front_connection_mapping)[*class_res->component_id]->insert(it.first->getId());
            }
            bool reset=false;
            long bcomp_els=front_resolver->getBiggestComponent()->getElements();
            long base_els=(baseline->getElements()<(2*cbNLimit) ? baseline->getElements() : (2*cbNLimit));
            if(bcomp_els>driftCheckingSizeRatio*base_els) reset=true;
            if(reset) {
                double mdw=front_resolver->meanWeightedComponentMDFrom(baseline);
                if(mdw>driftMovementMDThetaRatio*parent_resolver->getTheta()) {
                    if(eventCallback) eventCallback(new SHCEvent(DriftFrontTrigger,front_resolver,getId()));
                    obsolete=true;
                    if(parent_resolver!=NULL) parent_resolver->merge(front_resolver,this,front_connection_mapping);
                    delete front_resolver;
                    for(auto it:*front_connection_mapping) delete it.second;
                    delete front_connection_mapping;
                    front_resolver=NULL;
                    front_connection_mapping=NULL;
                } else {
                    delete front_resolver;
                    for(auto it:*front_connection_mapping) delete it.second;
                    delete front_connection_mapping;
                    front_resolver=new SHC(dim,AggresiveAgglomeration,NoDrift,true);
                    front_resolver->switchDecay(false);
                    front_connection_mapping=new unordered_map<string, std::set<string>*>();
                }
            }
        }
    }
    return true;
}

MatrixXd *SHC_Component::chooseUniOrMulti() {
    if(inverted) return cov;
    if(outlier) return vv;
    MatrixXd temp(dim,dim);
    temp.setZero();
    for(int i=0;i<cov->cols();i++)
        if((*cov)(i,i)!=(float)0.0) temp(i,i)=(*cov)(i,i);
    return new MatrixXd(temp.inverse());
}

double SHC_Component::mahalanobisDistance(VectorXd *newElement) {
    if(newElement==NULL) throw SHC_Component_Exception("New element must be defined");
    if(newElement->size()!=dim) {
        throw SHC_Component_Exception("New element does not have the same dimension as the component");
    }
    MatrixXd *icov=chooseUniOrMulti();
    VectorXd delta=*newElement-*mean;
    double N0=delta.transpose()*(*icov*delta);
    if(icov!=cov && icov!=vv) delete icov;
    return sqrtf(N0);
}

void SHC_Component::print(ostream &o_str) {
    o_str << "-=* Component " << getId() <<" *=-" << endl;
    o_str << "Elements:" << elements << " Dimension:" << dim << " Inverted:" << (inverted ? "true":"false") << " Outlier:" << (outlier ? "true":"false") << endl;
    o_str << "Mean:---" << endl << *mean << endl << "--------" << endl;
    o_str << "Covariance:---" << endl << (inverted ? cov->inverse() : *cov) << endl << "--------------" << endl;
}

VectorXd *SHC_Component::getMean() {
    return mean;
}

void SHC_Component::setMean(VectorXd *new_mean) {
    delete mean;
    mean=new_mean;
}

MatrixXd *SHC_Component::getCovariance() {
    if(!inverted) return new MatrixXd(*cov);
    else return new MatrixXd(cov->inverse());
}

void SHC_Component::setCovariance(MatrixXd *new_cov,bool new_inverted) {
    delete cov;
    if(new_inverted) cov=new MatrixXd(new_cov->inverse());
    else cov=new MatrixXd(*new_cov);
    inverted=new_inverted;
}

bool SHC_Component::isCovarianceInverted() {
    return inverted;
}

void SHC_Component::setObsolete(bool obsolete) {
    this->obsolete=obsolete;
}

bool SHC_Component::isObsolete() {
    return obsolete;
}

bool SHC_Component::isBlocked() {
    return blocked;
}

void SHC_Component::setBlocked(bool blocked) {
    this->blocked=blocked;
}

long SHC_Component::getElements() {
    return elements;
}

void SHC_Component::setElements(long new_elements) {
    this->elements=new_elements;
}

double SHC_Component::connectionMeasure(SHC_Component *comp,double theta) {
    double md1=mahalanobisDistance(comp->getMean());
    double p1=theta/md1;
    double md2=comp->mahalanobisDistance(this->getMean());
    double p2=theta/md2;
    if((p1+p2)==0) return (double)0.0;
    return 1/(p1+p2);
}

double SHC_Component::euclideanDistance(SHC_Component *comp) {
    VectorXd diff=*(comp->getMean())-*mean;
    double diff_sn=diff.squaredNorm();
    return diff_sn;
}

unordered_map<string, SHC_Containable *> SHC_Component::fetchChildComponents() {
    unordered_map<string, SHC_Containable *> res;
    res[getId()]=this;
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

SHC_Component::SHC_Component(SHC_Containable_Set *set,Eigen::MatrixXd *initDataSet, Eigen::VectorXd *virtualVariance,double cbVarianceLimit,long cbNLimit,
                             float driftCheckingSizeRatio,float driftMovementMDThetaRatio,int decayPace,float componentFormingMinVVRatio,
                             float componentBlockingLimitVVRatio) : SHC_Containable(set) {
    if(set!=NULL) set->addComponent(this);
    if(initDataSet==NULL) throw SHC_Component_Exception("Initial dataset must be defined");
    if(initDataSet->rows()<1) throw SHC_Component_Exception("Initial dataset must have at least one data element (row)");
    VectorXd firstElement=initDataSet->row(0);
    this->dim=firstElement.size();
    if(virtualVariance!=NULL && virtualVariance->size()!=dim) throw SHC_Component_Exception("Virtual variance vector does not have the same dimension as the new element");
    this->cov=new MatrixXd(MatrixXd::Zero(dim,dim));
    this->mean=new VectorXd(firstElement);
    MatrixXd tmp(dim,dim);
    tmp.setZero();
    if(virtualVariance==NULL)
        for(int i=0;i<dim;i++) tmp(i,i)=DEFAULT_VIRTUAL_VARIANCE;
    else {
        for(int i=0;i<dim;i++) tmp(i,i)=(*virtualVariance)(i);
        this->vv_value=(*virtualVariance)(0);
    }
    this->vv=new MatrixXd(tmp.inverse());
    this->elements=1;
    this->cbVarianceLimit=cbVarianceLimit;
    this->cbNLimit=cbNLimit;
    this->driftCheckingSizeRatio=driftCheckingSizeRatio;
    this->driftMovementMDThetaRatio=driftMovementMDThetaRatio;
    this->componentFormingMinVVRatio=componentFormingMinVVRatio;
    this->componentBlockingLimitVVRatio=componentBlockingLimitVVRatio;
    for(int i=1;i<initDataSet->rows();i++) {
        VectorXd el=initDataSet->row(i);
        addNewElement(&el,0,NULL);
    }
    this->decay_pace=decayPace;
    if(decay_pace>0 && cbNLimit>0) this->decay_handler=new SHC_Count_Decay(decay_pace*cbNLimit);
}

SHC_Component::SHC_Component(SHC_Containable_Set *set,VectorXd *mean,MatrixXd *covariance,bool inverted,long elements,
                             VectorXd *virtualVariance) : SHC_Containable(set) {
    if(set!=NULL) set->addComponent(this);
    if(mean==NULL || covariance==NULL) throw SHC_Component_Exception("Initial component parameters (mean,covariance) must be defined");
    this->dim=mean->size();
    if(virtualVariance!=NULL && virtualVariance->size()!=dim) throw SHC_Component_Exception("Virtual variance vector does not have the same dimension as the new element");
    this->cov=new MatrixXd(*covariance);
    if(inverted) this->inverted=true;
    else if(isCovInvertible()) {
        MatrixXd *tcov=this->cov;
        this->cov=new MatrixXd(tcov->inverse());
        this->inverted=true;
        delete tcov;
    }
    this->mean=new VectorXd(*mean);
    MatrixXd tmp(dim,dim);
    tmp.setZero();
    if(virtualVariance==NULL)
        for(int i=0;i<dim;i++) tmp(i,i)=DEFAULT_VIRTUAL_VARIANCE;
    else {
        this->vv_value=(*virtualVariance)(0);
        for(int i=0;i<dim;i++) tmp(i,i)=vv_value;
    }
    this->vv=new MatrixXd(tmp.inverse());
    this->elements=elements;
    this->outlier=elements==1;
}

SHC_Component::SHC_Component(SHC_Containable_Set *set,SHC_Component *cloneFrom) : SHC_Containable(set) {
    if(set!=NULL) set->addComponent(this);
    if(cloneFrom==NULL) throw SHC_Component_Exception("Cloning component from undefined component");
    this->source_node=cloneFrom->source_node;
    this->dim=cloneFrom->dim;
    this->cov=new MatrixXd(*cloneFrom->cov);
    if(cloneFrom->inverted) this->inverted=true;
    else if(isCovInvertible()) {
        MatrixXd *tcov=this->cov;
        this->cov=new MatrixXd(tcov->inverse());
        this->inverted=true;
        delete tcov;
    }
    this->mean=new VectorXd(*cloneFrom->mean);
    this->vv=new MatrixXd(*cloneFrom->vv);
    this->vv_value=cloneFrom->vv_value;
    this->elements=cloneFrom->elements;
    this->outlier=elements==1;
    if(cloneFrom->baseline!=NULL) this->baseline=cloneFrom->baseline->clone(NULL,true);
    if(cloneFrom->front_resolver!=NULL) this->front_resolver=new SHC(cloneFrom->front_resolver);
    this->cbVarianceLimit=cloneFrom->cbVarianceLimit;
    this->cbNLimit=cloneFrom->cbNLimit;
    this->obsolete=cloneFrom->obsolete;
    this->driftCheckingSizeRatio=cloneFrom->driftCheckingSizeRatio;
    this->driftMovementMDThetaRatio=cloneFrom->driftMovementMDThetaRatio;
    this->componentFormingMinVVRatio=cloneFrom->componentFormingMinVVRatio;
    this->blocked=cloneFrom->blocked;
    this->decay_pace=cloneFrom->decay_pace;
    if(cloneFrom->decay_handler!=NULL) this->decay_handler=cloneFrom->decay_handler->clone();
    this->trace.insert(cloneFrom->trace.begin(),cloneFrom->trace.end());
}

SHC_Component_Details *SHC_Component::calculateDetails(double theta) {
    return calculateDetails(theta, 0, 1, cbVarianceLimit, cbNLimit, true);
}

SHC_Component_Details *SHC_Component::calculateDetails(double theta,int dim1,int dim2,double cbVarianceLimit,double cbNLimit,bool single) {
    if(this->dim!=2) throw SHC_Component_Exception("Cannot calculate component details, the number of dimensions is not exactly 2");
    double R=0;
    int inc=5,vno=(!single ? (int)180/inc : (int)360/inc),rindex=0;
    MatrixXd *res1=NULL,*res2=NULL;
    if(!single) {
        res1=new MatrixXd(vno+1, 2);
        for(int angle(0);angle<=180;angle+=inc) {
            pair<double, VectorXd *> *r1=getEllipticalThresholdRadius(theta, angle, R, dim1, dim2);
            (*res1)(rindex,0)=(*r1->second)(dim1);
            (*res1)(rindex++,1)=(*r1->second)(dim2);
            R=r1->first;
            delete r1->second;delete r1;
        }
        rindex=0;
        R=0;
        res2=new MatrixXd(vno+1, 2);
        for(int angle(0);angle>=-180;angle-=inc) {
            pair<double, VectorXd *> *r2=getEllipticalThresholdRadius(theta, angle, R, dim1, dim2);
            (*res2)(rindex,0)=(*r2->second)(dim1);
            (*res2)(rindex++,1)=(*r2->second)(dim2);
            R=r2->first;
            delete r2->second;delete r2;
        }
    } else {
        res1=new MatrixXd(vno+1, 2);
        for(int angle(0);angle<=360;angle+=inc) {
            pair<double, VectorXd *> *r1=getEllipticalThresholdRadius(theta, angle, R, dim1, dim2);
            (*res1)(rindex,0)=(*r1->second)(dim1);
            (*res1)(rindex++,1)=(*r1->second)(dim2);
            R=r1->first;
            delete r1->second;delete r1;
        }
    }
    MatrixXd *tmp_cov=getCovariance();
    bool covTh=(tmp_cov->array()>cbVarianceLimit).any();
    delete tmp_cov;
    bool nTh=elements>cbNLimit;
    SHC_Component_Details *res=NULL;
    if(!single) res=new SHC_Component_Details(this, res1, res2, covTh, nTh, obsolete, new VectorXd(*mean), (baseline!=NULL ? new VectorXd(*baseline->getMean()) : NULL));
    else res=new SHC_Component_Details(this, res1, covTh, nTh, obsolete, new VectorXd(*mean), (baseline!=NULL ? new VectorXd(*baseline->getMean()) : NULL));
    /*SHC_Component *bcomp=NULL;
    if(front_resolver!=NULL && (bcomp=front_resolver->getBiggestComponent())!=NULL)
        res->childDetails.push_back(bcomp->calculateDetails(theta, dim1, dim2, cbVarianceLimit, cbNLimit, single));*/
    if(front_resolver!=NULL)
        for(pair<string,SHC_Component*> it:*front_resolver->getComponents())
            if(it.second->getElements()>1) res->childDetails.push_back(it.second->calculateDetails(theta, dim1, dim2, cbVarianceLimit, cbNLimit, single));
    if(baseline) res->baselineDetails=baseline->calculateDetails(theta, dim1, dim2, cbVarianceLimit, cbNLimit, single);
    return res;
}

VectorXd *SHC_Component::getEllipticalCoordinates(double angle,double R,int dim1,int dim2) {
    double radi=(2*M_PI*angle)/360;
    double x=(double)R*cos(radi),y=(double)R*sin(radi);
    VectorXd *el=new VectorXd(2);
    (*el)(0)=x+(*mean)(dim1);
    (*el)(1)=y+(*mean)(dim2);
    return el;
}

pair<double, VectorXd *> *SHC_Component::getEllipticalThresholdRadius(double theta,double angle,double initR,int dim1,int dim2) {
    double R=initR;
    VectorXd *pos=getEllipticalCoordinates(angle, R, dim1, dim2);
    double md=mahalanobisDistance(pos);
    if(md>=theta) {
        while(md>=theta) {
            R-=0.05;
            delete pos;
            pos=getEllipticalCoordinates(angle, R, dim1, dim2);
            md=mahalanobisDistance(pos);
        }
        return new pair<double, VectorXd*>(R, pos);
    } else {
        while(md<theta) {
            R+=0.05;
            delete pos;
            pos=getEllipticalCoordinates(angle, R, dim1, dim2);
            md=mahalanobisDistance(pos);
        }
        return new pair<double, VectorXd*>(R-0.05, pos);
    }
}

bool SHC_Component::isOutlier() {
    return outlier;
}

void SHC_Component::redirectComponent(SHC_Component *to_c) {
    // check if we don't have a redirection loop... if we do, do not allow redirection to be assigned
    if(to_c==NULL) {
        redirect_to=NULL;
        return;
    }
    if(this->redirect_to && this->redirect_to==to_c) return;
    SHC_Component *rto=to_c;
    while(rto!=NULL) {
        if(rto==this) return;
        rto=rto->getRedirectedComponent();
    }
    if(this->redirect_to) cleanRedirection();
    this->redirect_to=to_c;
    to_c->addFromRedirectedComponent(this);
}

void SHC_Component::addFromRedirectedComponent(SHC_Component *from_c) {
    if(redirected_from.find(from_c->getId())==redirected_from.end()) redirected_from[from_c->getId()]=from_c;
}
void SHC_Component::removeFromRedirectedComponent(SHC_Component *from_c) {
    if(redirected_from.find(from_c->getId())!=redirected_from.end()) redirected_from.erase(from_c->getId());
}
void SHC_Component::cleanRedirection() {
    for(pair<string, SHC_Component*> it:redirected_from)
        it.second->redirectComponent(NULL);
    redirected_from.erase(redirected_from.begin(),redirected_from.end());
}

bool SHC_Component::isRedirected() {
    return redirect_to!=NULL;
}

SHC_Component *SHC_Component::getRedirectedComponent() {
    return redirect_to;
}

SHC_Component *SHC_Component::getFinalRedirectedComponent() {
    if(isRedirected()) return redirect_to->getFinalRedirectedComponent();
    else return this;
}

bool SHC_Component::operator==(const SHC_Component &c) const {
    return parent->getId()==c.parent->getId();
}

bool SHC_Component::hasBaseline() {
    return baseline!=NULL;
}

SHC_Component *SHC_Component::getBaseline() {
    return baseline;
}

SHC_Component *SHC_Component::clone(SHC_Containable_Set *set_forClone, bool retainId) {
    SHC_Component *cloned_c=new SHC_Component(set_forClone,this);
    if(retainId) cloned_c->setId(getId());
    return cloned_c;
}

void SHC_Component::resetBaseline() {
    if(this->baseline!=NULL) delete this->baseline;
    this->baseline=NULL;
}

void SHC_Component::setBaselineLimits(double cbVarianceLimit, long cbNLimit) {
    this->cbVarianceLimit=cbVarianceLimit;
    this->cbNLimit=cbNLimit;
    if(decay_handler) decay_handler->reset(decay_pace*elements<decay_pace*cbNLimit ? decay_pace*cbNLimit : decay_pace*elements);
}

SHC *SHC_Component::getFrontResolver() {
    return front_resolver;
}

void SHC_Component::setEventCallback(function<void (SHCEvent*)> eventCallback) {
    this->eventCallback=eventCallback;
}

void SHC_Component::setDriftOptions(float sizeRatio,float movementRatio) {
    this->driftCheckingSizeRatio=sizeRatio;
    this->driftMovementMDThetaRatio=movementRatio;
}

bool SHC_Component::decayPing() {
    if(decay_handler) {
        bool r=decay_handler->ping();
        if(r) setObsolete(true);
        return r;
    }
    return false;
}

void SHC_Component::switchDecay(bool decay,int decayPace) {
    if(!decay && decay_handler) {
        delete decay_handler;
        decay_handler=NULL;
    }
    if(decay) decay_pace=decayPace;
    if(decay && !decay_handler && decay_pace>0) {
        decay_handler=new SHC_Count_Decay((outlier ? 20 : 1)*(decay_pace*elements<decay_pace*cbNLimit ? decay_pace*cbNLimit : decay_pace*elements));
    }
}

SHC_Count_Decay *SHC_Component::getDecayHandler() {
    return decay_handler;
}

void SHC_Component::setId(string id) {
    string old_id=this->getId();
    if(set!=NULL) set->removeContainer(&old_id);
    forcesetId(id);
    if(set!=NULL) set->addComponent(this);
}

long SHC_Component::getDimensions() {
    return this->dim;
}

void SHC_Component::addNeighbor(SHC_Component *neighbor,SHC *parent,bool measure) {
    if(neighbor==this) return;
    if(!neighbor->isOutlier()) {
        removeNeighbor(neighbor);
        return;
    }
    if(measure && parent!=NULL) {
        double md=mahalanobisDistance(neighbor->getMean());
        //parent->incCounter();
        if(md>2*parent->getTheta()) {
            removeNeighbor(neighbor);
            return;
        }
    }
    if(neighborhood.find(neighbor)==neighborhood.end()) neighborhood.insert(neighbor);
}

void SHC_Component::removeNeighbor(SHC_Component *neighbor) {
    std::set<SHC_Component*>::iterator it=neighborhood.find(neighbor);
    if(it!=neighborhood.end()) neighborhood.erase(it);
}

void SHC_Component::removeFromNeighborhood(SHC *parent) {
    for(pair<string,SHC_Component*> it:*parent->getComponents())
        it.second->removeNeighbor(this);
}

void SHC_Component::findNeighbors(SHC *parent) {
    if(agglo_out_blocker>0) return;
    double th=parent->getTheta();
    vector<SHC_Component*> *outs=parent->getOutliers();
    for(SHC_Component *out:*outs) {
        double md=mahalanobisDistance(out->getMean());
        //parent->incCounter();
        if(md<=th) parent->agglomerateComponents(this,out);
        else if(md>th && md<=2*th) addNeighbor(out);
    }
    delete outs;
    if(neighborhood.size()==0) agglo_out_blocker=400;
}

void SHC_Component::agglomerateNeighborhood(SHC *parent) {
    if(this->outlier || this->blocked) return;
    if(neighborhood.size()==0) findNeighbors(parent);
    if(neighborhood.size()==0) return;
    vector<SHC_Component*> *an=new vector<SHC_Component*>();
    double th=parent->getTheta();
    for(SHC_Component *neighbor:neighborhood) {
        double md=mahalanobisDistance(neighbor->getMean());
        //parent->incCounter();
        if(md<=th) an->push_back(neighbor);
    }
    for(SHC_Component *neighbor:*an) {
        parent->agglomerateComponents(this,neighbor);
        removeNeighbor(neighbor);
    }
    delete an;
}

set<SHC_Component*> *SHC_Component::getNeighborhood() {
    return &neighborhood;
}

void SHC_Component::setSourceNode(string new_source_node) {
    this->source_node=new_source_node;
}

string SHC_Component::getSourceNode() {
    return this->source_node;
}
