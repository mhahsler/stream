#include <Rcpp.h>
#include "NumericVector.h"
#include "dist.h"

// FIMXE: add attraction to serealization

namespace DStream_PKG {

// Micro-cluster
class MC {
public:
  double weight;
  int t;
  Rcpp::NumericVector attraction;  // empty attraction
  
  MC(double weight_, int t_) : weight(weight_), t(t_) {}
  MC() : weight(0.0), t(0) {}
  /*  ~MC() {
   Rcpp::Rcout << "Destroying " << std::endl;
  }
   */
};

std::ostream& operator<<(std::ostream& os, MC const& obj) {
  os << "MC with weight " << obj.weight << " and timestamp " << obj.t;
  return os;
}


void Rprint_grid(const std::vector<double>& obj) {
  Rcpp::Rcout << "[ ";
  for (std::vector<double>::const_iterator it = obj.begin(); it<obj.end(); ++it) 
    Rcpp::Rcout << *it << " ";
  Rcpp::Rcout << "]";
}

class DStream {
public:
  DStream(double gridsize_, double decay_factor_, int gap_time_,
    double Cl_, double N_, bool attraction_, double epsilon_) :
  gridsize(gridsize_),
  decay_factor(decay_factor_),
  gap_time(gap_time_),
  Cl(Cl_),
  N(N_),
  attraction(attraction_),
  epsilon(epsilon_){
    t = 0;
    N_fixed = N!=0; // N=0 means that we update N
    d = 0; // d is unknown
  }
  
  
  // Unserialization constructor
  DStream(SEXP all_) {
    Rcpp::List all(all_);
    
    gridsize = all["gridsize"];
    d = all["d"];
    decay_factor  = all["decay_factor"];
    gap_time = all["gap_time"];
    Cl = all["Cl"];
    N = all["N"];
    N_fixed = all["N_fixed"];
    attraction = all["attraction"];
    epsilon = all["epsilon"];
    t = all["t"];
    cell_volume = all["cell_volume"];
    eps = all["eps"];
    cube_volume = all["cube_volume"];
    
    mins = all["mins"]; 
    maxs = all["maxs"]; 
    
    Rcpp::NumericVector w = all["mcs_weights"];
    Rcpp::NumericMatrix c = all["mcs_centers"];
    
    // recreate MCs
    for(int i=0; i<w.length(); i++) { 
      Rcpp::NumericVector grid = c(i, Rcpp::_);
      std::vector<double> gg = Rcpp::as<std::vector<double> >(grid);
      mcs.insert(std::pair<std::vector<double>, MC>(gg, MC(w(i), t)));
    } 
    // add attraction 
  }
  
  Rcpp::List serializeR(){
    return Rcpp::List::create(
      
      Rcpp::Named("type") = "stream::DStream",
      Rcpp::Named("gridsize") = gridsize,
      Rcpp::Named("d") = d,
      Rcpp::Named("gap_time") = gap_time,
      Rcpp::Named("Cl") = Cl,
      Rcpp::Named("decay_factor") = decay_factor,
      Rcpp::Named("N") = N,
      Rcpp::Named("N_fixed") = N_fixed,
      Rcpp::Named("attraction") = attraction,
      Rcpp::Named("epsilon") = epsilon,
      Rcpp::Named("t") = t,
      Rcpp::Named("cell_volume") = cell_volume,
      Rcpp::Named("eps") = eps,
      Rcpp::Named("cube_volume") = cube_volume,
      Rcpp::Named("mins") = mins,
      Rcpp::Named("maxs") = maxs,
    
      Rcpp::Named("mcs_centers") = getCenters(FALSE),
      Rcpp::Named("mcs_weights") = getWeights()
      // attraction
    );
  } 
  
  
  int nClusters() { return(mcs.size()); }
  
  // FIXME: Rcpp does currently not support default values for methods!
  //Rcpp::NumericMatrix getCenters(bool decode = TRUE) {
  Rcpp::NumericMatrix getCenters(bool decode) {
    int n = mcs.size();
    Rcpp::NumericMatrix v(n, d);
    
    if(n>0) {
      std::map<std::vector<double>, MC>::iterator it;
      
      int i=0;
      for(it = mcs.begin(); it != mcs.end(); ++it, i++) {
        Rcpp::NumericVector xx(it->first.begin(), it->first.end());
        if(decode) xx = xx * gridsize + gridsize/2.0;
        v(i, Rcpp::_) = xx;
      }
    }
    return v;
  }
  
  Rcpp::NumericVector getWeights() {
    int n = mcs.size();
    Rcpp::NumericVector v(n);
    std::map<std::vector<double>, MC>::iterator it;
    
    int i=0;
    for(it = mcs.begin(); it != mcs.end(); ++it, i++)
      v(i) = it->second.weight * pow(decay_factor, t-it->second.t);
    
    return v;
  }
  
  Rcpp::NumericMatrix getAttraction() {
    // FIXME: check if attraction!
    // FIXME: as dist!
    int n = mcs.size();
    Rcpp::NumericMatrix m(n, n);
    std::map<std::vector<double>, MC>::iterator it;
    
    // mc id lookup table
    std::map<std::vector<double>, int> ids;
    int i=0;
    for(it = mcs.begin(); it != mcs.end(); ++it, i++) ids[it->first]=i;
    
    i=0;
    for(it = mcs.begin(); it != mcs.end(); ++it, i++) {
      for(int j=0; j<d; j++) {
        std::vector<double> adjacent = it->first;
        adjacent[j]-= 1;
        if(ids.count(adjacent)) m(ids[it->first], ids[adjacent]) = it->second.attraction(j*2);
        adjacent[j] += 2;
        if(ids.count(adjacent)) m(ids[it->first], ids[adjacent]) = it->second.attraction(j*2+1);
      }
    }
    
    return m;
  }
  
  // update
  // FIXME: Rcpp does currently not support default values for methods!
  //void update(Rcpp::NumericMatrix& data, bool debug=FALSE) {
  void update(Rcpp::NumericMatrix& data, bool debug) {
    int n = data.nrow();
    std::map<std::vector<double>, MC>::iterator it;
    
    for (int ii=0; ii<n; ii++) {
      t++;
      
      if(!(t%gap_time) && decay_factor<1) {
        Rcpp::checkUserInterrupt();
        
        if(debug) Rcpp::Rcout << "cleanup at point " << ii+1 << " @ t = " 
                              << t << ":"<< std::endl;
        
        if(mcs.size()>0) {
          
          // remove sporadic grids
          std::map<std::vector<double>, MC>::iterator it;
          
          // update N
          if(!N_fixed) {
            Rcpp::NumericVector minmax = maxs-mins+1;
            N = std::accumulate(minmax.begin(),
              minmax.end(), 1.0, std::multiplies<double>());
            
            if(debug) Rcpp::Rcout << "\tupdated N to " << N << std::endl;
          }
          
          
          double df;
          it = mcs.begin();
          while(it != mcs.end()) {
            df = pow(decay_factor, t-it->second.t);
            if(it->second.weight * df
              < Cl*(1- df*decay_factor)/(N*(1-decay_factor))) {
              
              if(debug){ 
                Rcpp::Rcout << "\tdeleting MC @ "; 
                Rprint_grid(it->first);
                Rcpp::Rcout << std::endl;
              }
              mcs.erase(it++); // iterator becomes invalid after erase!
            } else ++it;
          }
        }
        
        if(debug) Rcpp::Rcout << "\tMC left: " << mcs.size() << std::endl;
        
      }
      
      // process new data point
      Rcpp::NumericVector p = data(ii, Rcpp::_);
      
      //FIXME: check d (also in DBSTREAM)
      
      // find grid cell and insert point
      Rcpp::NumericVector grid = floor(p/gridsize);
      
      // first data point (guess dimensions)
      if(t==1) {
        d = p.length();
        
        // min/max
        mins = Rcpp::clone(grid);
        maxs = Rcpp::clone(grid);
        
        // for attraction
        cell_volume = pow(gridsize, d);
        eps = gridsize * epsilon;
        cube_volume = pow(eps, d);
      }
      
      // update min/max  
      mins = Rcpp::pmin(mins, grid);
      maxs = Rcpp::pmax(maxs, grid);
      
      std::vector<double> gg = Rcpp::as<std::vector<double> >(grid);
      MC& mc = mcs[gg];
      if(mc.t>0) mc.weight *= pow(decay_factor, t-mc.t);
      mc.weight++;
      
      if(debug) {
        Rcpp::Rcout << "point " << ii << ": updating grid @ ";
        Rprint_grid(gg);
        Rcpp::Rcout << " to " << mc.weight << std::endl;
      }
      
      
      // Update attraction
      if(attraction) {
        Rcpp::NumericVector center = grid * gridsize + gridsize/2;
        int prev_or_next = 0;
        double b = 0;
        
        // create or age attraction
        if(mc.attraction.length()==0) mc.attraction = Rcpp::NumericVector(2*d);
        else mc.attraction = mc.attraction * pow(decay_factor, t-mc.t);
        
        // go over all adjacent cells
        for(int k=0; k<d; k++) {
          if(center(k) > p(k)) prev_or_next = 0;  // prev
          else prev_or_next = 1;                  // next
          
          b = eps-(gridsize/2.0 - std::abs(center(k) - p(k)));
          if(b>0) {
            for(int i=0; i<d; i++) {
              if(i==k) continue;
              b*= std::min(gridsize/2.0 - std::abs(center(i)-p(i)), eps/2.0) + eps/2.0;
            }
          } else b=0;
          
          mc.attraction(2*k+prev_or_next) += b;
        }
        if(debug) Rcpp::Rcout << "\tattraction updated to " << 
          mc.attraction << std::endl; 
        
        // update timestamp
      }
      
      // back to main  
      mc.t = t;
    }
  }
  
  double gridsize;
  int d;
  double decay_factor;
  int gap_time;
  double Cl;
  double N;
  bool N_fixed;
  bool attraction;
  double epsilon;
  std::map<std::vector<double>, MC> mcs;
  //  std::map<std::pair<unsigned int,unsigned int>, Rel> rel;
  int t;
  Rcpp::NumericVector mins;
  Rcpp::NumericVector maxs;
  
private:
  double cell_volume;
  double eps;             // gridsize*epsilon
  double cube_volume;
  
};

using namespace Rcpp ;
RCPP_MODULE(MOD_DStream){
  
  class_<DStream>("DStream")
  .constructor<double, double, int, double, double, bool, double>()
  .constructor<SEXP>()
  
  .field_readonly("gridsize", &DStream::gridsize)
  .field_readonly("decay_factor", &DStream::decay_factor)
  .field_readonly("gap_time", &DStream::gap_time)
  .field_readonly("t", &DStream::t)
  .field_readonly("N", &DStream::N)
  .field_readonly("mins", &DStream::mins)
  .field_readonly("maxs", &DStream::maxs)
  
  
  .method("centers", &DStream::getCenters)
  .method("weights", &DStream::getWeights)
  .method("nClusters", &DStream::nClusters)
  .method("getAttraction", &DStream::getAttraction)
  .method("update", &DStream::update)
  .method("serializeR", &DStream::serializeR)
  ;
}

}

/**** R
DStream
s <- 1e6
#d <- matrix(runif(n = 2*s), nrow=s, byrow = T)
d <- matrix(rnorm(n = 2*s, sd=.5), nrow=s, byrow = T)

#double gridsize_, double decay_factor_,
#int gap_time_, double Cl, bool attraction
n <- new(DStream, .1, 2^-1e-3, 1000, .8, 2*6/.1, FALSE, .3)
n$t

system.time(n$update(d))
n$t
#n$centers
ws <- n$weights()
ws
cen <- n$centers()
plot(cen)
str(cen)
n$nClusters()
plot(cen, pch=15, col=gray(1-ws/max(ws)))


s <- 1e5
#d <- matrix(runif(n = 2*s), nrow=s, byrow = T)
d <- matrix(rnorm(n = 2*s, sd=.5), nrow=s, byrow = T)

n <- new(DStream, .1, 2^-1e-3, 1000, .8, 2*6/.1, TRUE, .3)
n$t

system.time(n$update(d))
n$t
n$nClusters()
ws <- n$weights()
hist(ws)
cen <- n$centers()
plot(cen, pch=15, col=gray(1-ws/max(ws)))

att <- n$getAttraction()
image(att)

*/

