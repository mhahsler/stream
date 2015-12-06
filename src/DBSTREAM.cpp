#include <Rcpp.h>
#include "NumericVector.h"
#include "dist.h"


// FIXME: check if id or t is running out!

namespace DBSTREAM_PKG {

enum dist_metric { EUCLIDEAN = 0, MANHATTAN = 1, MAXIMUM = 2 };

// Micro-cluster
class MC {
public:
  int id;
  double weight;
  Rcpp::NumericVector center;
  int t;
  
  MC(int id_, double weight_, Rcpp::NumericVector center_, int t_) :
    id(id_), weight(weight_), center(center_), t(t_) {}
};

std::ostream& operator<<(std::ostream& os, MC const& obj) {
  os << "MC " << obj.id << " @ " << obj.center 
     << " (weight = " << obj.weight << ", t = " << obj.t << ")";
  return os;
}


// relationship for shared density
class Rel {
public:
  double weight;
  int t;
  
  Rel(double weight_, int t_) : weight(weight_), t(t_) {}
  Rel() : weight(0), t(0) {}
};


class DBSTREAM {
public:
  DBSTREAM(double r_, double decay_factor_, int gap_time_,
    bool shared_, double alpha_, int metric_ = 0) :
  r(r_),
  decay_factor(decay_factor_),
  gap_time(gap_time_),
  shared(shared_),
  alpha(alpha_) {
    w_min = pow(decay_factor, gap_time);
    w_removed = 0.0;
    t = 0;
    topID = 1;
    metric = (dist_metric) metric_;   // 0 = Euclidean
  }
  
  // Deserialization constructor
  //DBSTREAM(Rcpp::List& all) {
  DBSTREAM(SEXP all_) {
    Rcpp::List all(all_);
    
    r = all["r"];
    decay_factor = all["decay_factor"];
    gap_time = all["gap_time"];
    shared = all["shared"];
    alpha = all["alpha"];
    t = all["t"];
    w_min = all["w_min"];
    topID = all["topID"];
    metric = (dist_metric) (int) all["metric"];
    
    Rcpp::NumericVector w = all["mcs_weights"]; 
    Rcpp::NumericMatrix c = all["mcs_centers"];
    Rcpp::NumericMatrix rr = all["rels"];
    Rcpp::NumericVector ids = c.attr("ids");
    
    // recreate MCs
    for(int i=0; i<w.length(); i++) 
      mcs.push_back(MC(ids(i), w(i), c(i, Rcpp::_), t));
    
    // recreate Rels
    for(int i=0; i<rr.nrow(); i++) {
      Rcpp::NumericVector r = rr(i, Rcpp::_);
      rel[std::make_pair(r(0), r(1))] = Rel(r(2), t);
    }
  }
  
  Rcpp::List serializeR(){
    return Rcpp::List::create(
      Rcpp::Named("type") = "stream::DBSTREAM",
      Rcpp::Named("r") = r,
      Rcpp::Named("decay_factor") = decay_factor,
      Rcpp::Named("gap_time") = gap_time,
      Rcpp::Named("shared") = shared,
      Rcpp::Named("alpha") = alpha,
      Rcpp::Named("t") = t,
      Rcpp::Named("w_min") = w_min,
      Rcpp::Named("topID") = topID,
      Rcpp::Named("metric") = (int) metric,
      Rcpp::Named("mcs_centers") = getCenters(),
      Rcpp::Named("mcs_weights") = getWeights(),
      Rcpp::Named("rels") = getRel()
    );
  }
  
  int nClusters() { return(mcs.size()); }
  
  Rcpp::NumericMatrix getCenters() {
    int n = mcs.size();
    if(n==0) return(Rcpp::NumericMatrix(0, 0));
    
    int d = mcs[0].center.length();
    
    Rcpp::IntegerVector ids(n);
    Rcpp::NumericMatrix m(n, d);
    for(int i=0; i<n; i++) {
      m(i, Rcpp::_) = mcs[i].center;
      ids(i) = mcs[i].id;
    }
    
    m.attr("ids") = ids;
    return m;
  }
  
  Rcpp::NumericVector getWeights() {
    int n = mcs.size();
    
    Rcpp::NumericVector v(n);
    for(int i=0; i<n; i++) v(i) = mcs[i].weight * pow(decay_factor, t - mcs[i].t);
    return v;
  }
  
  Rcpp::NumericMatrix getRel() {
    Rcpp::NumericMatrix m(rel.size(), 3);
    std::map<std::pair<int,int>, Rel>::iterator it;
    int i=0;
    
    for(it = rel.begin(); it!=rel.end(); ++it, ++i) {
      m(i, 0) = it->first.first;
      m(i, 1) = it->first.second;
      m(i, 2) = it->second.weight * pow(decay_factor, t - it->second.t);
    }
    return m;
  }
  
  Rcpp::NumericVector getSharedDensity() {
    int n = mcs.size();
    Rcpp::NumericMatrix rel = getRel();
    Rcpp::NumericVector dist(LT_SIZE(n));
    //Rcpp::NumericMatrix m(n, n);
    std::map<int,int> trans = getIDTrans();
    int from, to;
    
    for(int i=0; i<rel.nrow(); i++){
      from = trans[rel(i, 0)];
      to = trans[rel(i, 1)];
      
      // check for removed MCs (get 0)
      if(!from || !to) continue;
      
      dist(LT_POS(n, from-1, to-1)) = rel(i, 2);
    }
    
    dist.attr("Size") = n;
    dist.attr("Diag") = false;
    dist.attr("Upper") = false;
    dist.attr("method") = "sharedDensity";
    dist.attr("class") = "dist";
    return dist;
  }
  
  
  
  // update
  void update(Rcpp::NumericMatrix& data, 
    bool debug = FALSE, bool assignments = FALSE) {
    
    int n = data.nrow();
    std::vector<MC>::iterator it;
    
    if (assignments) last = Rcpp::IntegerVector(n);
    else last = Rcpp::IntegerVector(0);   // clear last
    
    for (int ii=0; ii<n; ii++) {
      t++;
      
      // decay weights
      // Is now done during update
      //for(it=mcs.begin(); it<mcs.end(); ++it) it->weight *= decay_factor;
      
      if(!(t%gap_time) && decay_factor<1) {
        Rcpp::checkUserInterrupt();
        
        if(debug) Rcpp::Rcout << "cleanup at point " << ii+1 << " @ t = " 
                              << t << ":"<< std::endl;
        
        if(debug) Rcpp::Rcout << "\t> MCs: " << mcs.size() << std::endl;
        if(debug) Rcpp::Rcout << "\t> Rels: " << rel.size() << std::endl;
        
        w_removed *= pow(decay_factor, gap_time);
        
        // remove clusters that reached 1 and then did not get at least one
        // point since last gap time
        std::set<int> removedMCs;
        it = mcs.begin();
        while(it != mcs.end()) {
          if(it->weight * pow(decay_factor, t - it->t) <= w_min) {
            
            if(debug) Rcpp::Rcout << "\terase "
                                  << *it << std::endl;
            
            // for removing relations
            removedMCs.insert(it->id);
            
            w_removed += it->weight * pow(decay_factor, t - it->t);
            it = mcs.erase(it);  // erase deletes the object and moves the iterator
          } else ++it;
        }
        
        if(debug) Rcpp::Rcout << "\t> MCs removed: " <<  removedMCs.size() 
          << std::endl;
        if(debug) Rcpp::Rcout << "\t> Weight of removed MCs: " <<  w_removed 
          << std::endl;
        
        // remove weak relationships
        
        if(shared) {
          std::map<std::pair<int,int>, Rel>::iterator it_rel;
          
          it_rel = rel.begin();
          while(it_rel != rel.end()){
            if(removedMCs.count(it_rel->first.first) 
              || removedMCs.count(it_rel->first.second)) {
              
              if(debug) Rcpp::Rcout << "\terase relation because of MC ("
                                    << it_rel->first.first << ", "
                                    << it_rel->first.second
                                    << ")" << std::endl;
              rel.erase(it_rel++);  // it does not move to the next pos. (map)
              
            } else
              
              if (it_rel->second.weight * pow(decay_factor, t - it_rel->second.t)
                < w_min * alpha) {
                
                if(debug) Rcpp::Rcout << "\terase weak relation ("
                                      << it_rel->first.first << ", "
                                      << it_rel->first.second
                                      << ")" << std::endl;
                rel.erase(it_rel++);  // it does not move to the next pos. (map)
              } else ++it_rel;
          }
        }
        
        if(debug) Rcpp::Rcout << "\t> MCs left: " << mcs.size() << std::endl;
        if(debug) Rcpp::Rcout << "\t> Rels left: " << rel.size() << std::endl;
      }
      
      // process new data point
      if (debug) Rcpp::Rcout << "point " << ii+1 << ":" << std::endl;
      Rcpp::NumericVector p = data(ii, Rcpp::_);
      
      // first cluster
      if (!mcs.size()) {
        if (assignments) last[ii] = topID;
        mcs.push_back(MC(topID++, 1.0, p, t));
        
      } else {
        
        // find neighbors and try to move
        std::vector<int> inside; //inside.reserve(dist.size());
        std::vector<Rcpp::NumericVector> new_centers;
        Rcpp::NumericVector dist = center_dist(p);
        
        for (int j = 0; j < dist.length(); j++) {
          if (dist[j] > r) continue;
          inside.push_back(j);
          // Move centers
          // Gaussian neighborhood function
          // Gaussian: h(j, i(x)) = exp(−||r_j − r_i(x)||^2 /2sigma^2 )
          // sigma = neighborhood radius : 2 sd deviaitons
          // sigma = sqrt(1/3/2)
          // [1] 0.4082483
          double partialweight = exp(-pow(dist[j]/r*3.0, 2.0) /2.0);
          //double partialweight = exp(-dist[j]/r * 3);
          
          // SOM style update: Wv(s + 1) = Wv(s) + thea(u, v, s) alpha(s)(D(t) - Wv(s)),
          // where  alpha(s) is a monotonically decreasing learning coefficient
          // and D(t) is the input vector; theta(u, v, s) is the neighborhood function
          if (debug) Rcpp::Rcout << "\ttry to move " << mcs[j];
          
          Rcpp::NumericVector nc = Rcpp::clone(mcs[j].center);
          nc = nc + partialweight * (p - nc);
          new_centers.push_back(nc);
          
          if (debug) Rcpp::Rcout << " to " << nc << std::endl;
          
        }
        
        if (inside.empty()) {
          if(assignments) last[ii] = topID;
          // create new cluster
          mcs.push_back(MC(topID++, 1.0, p, t));
          
            
          if(debug) Rcpp::Rcout << "\tnew MC at "
                               << p << std::endl;
          
        } else {
         
          // update weight 
          
          // all get the same weight
          //double w = 1.0/inside.size();
          for(std::vector<int>::iterator j = inside.begin(); 
            j != inside.end(); ++j) {
            
            MC& mc = mcs[*j];
            
            mc.weight *= pow(decay_factor, t - mc.t);
            mc.weight++;
            //mc.weight += w;
            mc.t = t;
          }
          
          if(assignments) {
            // find winner
            Rcpp::NumericVector ds = dist[Rcpp::IntegerVector(inside.begin(), 
              inside.end())];
            int min = std::min_element(ds.begin(), ds.end()) - ds.begin(); 
            last[ii] = mcs[inside[min]].id;
          }
          
          /*
          // only the largest gets updated
          MC& winner= mcs[*inside.begin()];
          for(std::vector<int>::iterator j = inside.begin()+1; 
            j != inside.end(); ++j) {
            
            MC& mc = mcs[*j];
            if(mc.weight > winner.weight) winner = mc;
          }  
            
          winner.weight *= pow(decay_factor, t - winner.t);
          winner.weight++;
          winner.t = t;
          */
          
          
          // update cluster position? 
          if (check_dist(new_centers)) {
            for (std::size_t i = 0; i < new_centers.size(); i++) 
              mcs[inside[i]].center = new_centers[i];
            
            if (debug) 
              Rcpp::Rcout << "\tNO COLLISIONS - centers moved!" << std::endl;
          }
          
          // Update relations
          if (shared && inside.size() >1) {
            for(std::size_t i=0; i<(inside.size()-1); i++) {
              for(std::size_t j=i+1; j<inside.size(); j++) {
                Rel& rr = rel[std::make_pair(mcs[inside[i]].id, mcs[inside[j]].id)];
                if(rr.t>0) rr.weight *= pow(decay_factor, t-rr.t);
                rr.t = t;
                rr.weight++;
                //rr.weight+=.5;
                //rr.weight += w;
              }
            }
          }
        }
      }
    }
  }
  
  double r;
  double decay_factor;
  int gap_time;
  bool shared;
  double alpha;
  std::vector<MC> mcs;
  // Note: key is always sorted: low/high id
  std::map<std::pair<int,int>, Rel> rel;
  double w_min;
  double w_removed;           // weight removed
  int t;
  int topID;                  // id for the next MC
  Rcpp::IntegerVector last;   // MC assignment of the recently clustered points 
  dist_metric metric;
  
private:
  
  // calculate distance of point p to all centers.
  inline Rcpp::NumericVector center_dist(Rcpp::NumericVector& p) {
    
    int n = mcs.size();
    
    Rcpp::NumericVector dist(n);
    
    if(metric == EUCLIDEAN) 
      for (int i = 0; i < n; i++)  
        dist[i] = sqrt(sum(pow(p - mcs[i].center, 2)));
    
    else if(metric == MANHATTAN)
      for (int i = 0; i < n; i++)  
        dist[i] = sum(abs(p - mcs[i].center));
    
    else { // MAXIMUM
      Rcpp::NumericVector diff;
      for (int i = 0; i < n; i++) {  
        diff = abs(p - mcs[i].center);
        dist[i] = *std::max_element(diff.begin(), diff.end());
      }
    }
    
    return dist;
  }
  
  // check if all points are at least factor * r apart from each other
  inline bool check_dist(std::vector<Rcpp::NumericVector> ps) {
   
   std::size_t i, j;
   double factor = .9;
    
    if (metric == EUCLIDEAN) {
      for (i = 0; i < (ps.size()-1); i++) 
        for (j = i+1; j < ps.size(); j++) 
          if (sqrt(sum(pow(ps[i] - ps[j], 2.0))) < r * factor) return false;
          
    } else if (metric == MANHATTAN) {
      for (i = 0; i < (ps.size()-1); i++) 
        for (j = i+1; j < ps.size(); j++) 
          if (sum(abs(ps[i] - ps[j])) < r * factor) return false;
          
    } else { // MAXIMUM 
      Rcpp::NumericVector diff;
      for (i = 0; i < (ps.size()-1); i++) 
        for (j = i+1; j < ps.size(); j++) {
          diff = abs(ps[i] - ps[j]);
          if (*std::max_element(diff.begin(), diff.end()) < r * factor) 
            return false;
        }
    }          
    
    return true;
  }
  

  // Conversions from MC ID to index
  std::map<int,int> getIDTrans() {
    int n = mcs.size();
    std::map<int,int> trans;
    
    // Hint we use id+1 since 0 is reserved for not found
    for(int i=0; i<n; i++) trans[mcs[i].id] = i+1;
    
    return trans;
  }
  
};


//RCPP_EXPOSED_CLASS(DBSTREAM)
using namespace Rcpp ;
RCPP_MODULE(MOD_DBSTREAM){
  
  class_<DBSTREAM>("DBSTREAM")
  // expose the default constructor
  .constructor<double, double, int, bool, double, int>()
  .constructor<SEXP>()
  
  .field_readonly("r", &DBSTREAM::r)
  .field_readonly("decay_factor", &DBSTREAM::decay_factor)
  .field_readonly("gap_time", &DBSTREAM::gap_time)
  .field_readonly("t", &DBSTREAM::t)
  .field_readonly("w_removed", &DBSTREAM::w_removed)
  .field("alpha", &DBSTREAM::alpha)
  .field_readonly("last", &DBSTREAM::last)
  
  .method("centers", &DBSTREAM::getCenters)
  .method("weights", &DBSTREAM::getWeights)
  .method("rels", &DBSTREAM::getRel)
  .method("nClusters", &DBSTREAM::nClusters)
  .method("getSharedDensity", &DBSTREAM::getSharedDensity)
  .method("update", &DBSTREAM::update)
  
  .method("serializeR", &DBSTREAM::serializeR)
  ;
}

}

/*** R
set.seed(1234) 

stream:::DBSTREAM
s <- 1e4
d <- rbind(matrix(rnorm(n = s, mean=c(-1,-1), sd=.3), ncol=2),
  matrix(rnorm(n = s, mean = c(1,1), sd=.3), ncol=2))
d <- d[sample(nrow(d)),]

n <- new(stream:::DBSTREAM, .1, 2^-0.01, 100, T, 0.2)
n$t
n$update(d[1:1000,], TRUE)
n$t

n <- new(stream:::DBSTREAM, .1, 2^-0.01, 1000, T, 0.2)
system.time(n$update(d, FALSE))

w <- n$weights()
str(w)
cen <- n$centers()
str(cen)

plot(cen)
plot(cen, cex=2, pch=16, col = gray((1-w/max(w))*.8, alpha=.5))

str(n$rels())
head(n$rels())
sd <- n$getSharedDensity()
image(as.matrix(sd))
*/

