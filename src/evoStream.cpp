#include <Rcpp.h>
#include <limits>
#include <ctime>
#include <random>
#include <algorithm>

#define VERBOSE 0

using namespace Rcpp;

// Unused: C++11 now uses std::shuffle - MFH
// inline int randWrapper(const int n) { return floor(unif_rand()*n); }


class MC {
public:
  Rcpp::NumericVector centroid;
  int lastUpdate;
  double weight;

  MC(Rcpp::NumericVector centroid, int lastUpdate, double weight) {
    this->centroid=centroid;
    this->lastUpdate=lastUpdate;
    this->weight=weight;
  }

  MC(Rcpp::NumericVector centroid, int lastUpdate) {
    this->centroid=centroid;
    this->lastUpdate=lastUpdate;
    this->weight=1;
  }

  Rcpp::NumericVector getCentroid(){
    return(centroid);
  }

  void merge(MC mc, int t, double lambda, double r) {
    mc.fade(t, lambda);
    this->fade(t, lambda);

    // update statistics
    this->weight += mc.weight;

    // competetive learning
    double d = this->distance(mc);
    this->centroid += exp(-pow(d/r*3.0, 2.0) /2.0) * (mc.getCentroid() - this->getCentroid());
  }


  void fade(int t, double lambda){
    // apply fading
    this->weight *= pow(2,(-lambda * (t-this->lastUpdate)));
    // update time
    this-> lastUpdate = t;
  }

  double distance(MC mc){
    return(sqrt(sum(pow(this->getCentroid() - mc.getCentroid(), 2))));
  }

  double distance(Rcpp::NumericVector x){
    return(sqrt(sum(pow(this->getCentroid() - x, 2))));
  }

};

class EvoStream {
public:

  double r;
  double lambda;
  int tgap;
  unsigned int k;
  double crossoverRate;
  double mutationRate;
  unsigned int populationSize;
  unsigned int initializeAfter;
  int incrementalGenerations;
  int reclusterGenerations;

  double omega;
  int t;
  int init;
  int upToDate;
  double delay;
  int initTime;
  std::vector<MC> micro;
  std::vector<Rcpp::NumericMatrix> macro;
  Rcpp::NumericVector macroFitness;


  // since exposed constructors have a limited number of parameters, we expose a setter function and use the default constructor
  void setFields(double r, double lambda, int tgap, unsigned int k, double crossoverRate, double mutationRate, int populationSize, unsigned int initializeAfter, int incrementalGenerations, int reclusterGenerations){
    this->r=r;
    this->lambda=lambda;
    this->tgap=tgap;
    this->k=k;
    this->crossoverRate=crossoverRate;
    this->mutationRate=mutationRate;
    this->populationSize=populationSize;
    this->initializeAfter=initializeAfter;
    this->incrementalGenerations = incrementalGenerations;
    this->reclusterGenerations = reclusterGenerations;

    this->macroFitness = Rcpp::NumericVector(this->populationSize);
    this->omega = pow(2, (-1*lambda * tgap));
    this->t=0;
    this->upToDate=0;
  }


  // constructor for stand-alone offline component
  void reclusterInitialize(Rcpp::NumericMatrix micro, Rcpp::NumericVector weights, unsigned int k, double crossoverRate, double mutationRate, int populationSize){
    this->k=k;
    this->crossoverRate=crossoverRate;
    this->mutationRate=mutationRate;
    this->populationSize=populationSize;
    this->macroFitness = Rcpp::NumericVector(this->populationSize);

    for(int i=0; i<micro.nrow(); i++){
      MC mc(micro(i,_), 1, weights(i));
      this->micro.push_back(mc);
    }

    this->initialize();
  }


  // RCpp Interface

  Rcpp::NumericMatrix get_microclusters(){
    int d = this->ndimensions();
    Rcpp::NumericMatrix x(this->micro.size(),d);
    for(unsigned int i=0; i<this->micro.size();i++){
      Rcpp::NumericVector mc = this->micro[i].getCentroid();
      for(int j=0; j<mc.size();j++){
        x(i,j)=mc[j];
      }
    }
    return(x);
  }


  Rcpp::NumericVector get_microweights(){
    Rcpp::NumericVector x(this->micro.size());
    for(unsigned int i=0; i<this->micro.size();i++){
      x[i]=this->micro[i].weight;
    }
    return(x);
  }


  Rcpp::NumericMatrix get_macroclusters(){

    if(!this->init){
      return(Rcpp::NumericMatrix(0,this->ndimensions()));
    }

    if(this->reclusterGenerations!=0 && this->upToDate==0){
      this->recluster(reclusterGenerations);
      this->upToDate=1;
    }

    // find max fitness
    int maxIdx=-1;
    double max=-1*std::numeric_limits<double>::max();

    for(unsigned int i=0; i<this->macroFitness.size(); i++){
      if(this->macroFitness[i]>max){
        max=this->macroFitness[i];
        maxIdx = i;
      }
    }
    return(this->macro[maxIdx]);
  }


  Rcpp::NumericVector get_macroweights(){

    if(!this->init){
      return(Rcpp::NumericVector(0));
    }
    if(reclusterGenerations!=0 && this->upToDate==0){
      this->recluster(reclusterGenerations);
      this->upToDate=1;
    }

    Rcpp::IntegerVector clusterAssignment = this->microToMacro();
    Rcpp::NumericVector microWeights = this->get_microweights();
    Rcpp::NumericVector macroWeights(this->k);

    // for every cluster
    for(int i=0; i<clusterAssignment.size(); i++){
      macroWeights(clusterAssignment(i)) += microWeights[i];
    }

    return(macroWeights);
  }


  Rcpp::IntegerVector microToMacro(){

    if(!this->macro.size()){
      return(Rcpp::IntegerVector(0));
    }

    Rcpp::IntegerVector assignment = this->getAssignment(this->get_macroclusters());
    return(assignment);

  }

  // Online component


  void cluster(Rcpp::NumericMatrix data){

    this->upToDate=0;

    for(int i=0; i<data.nrow(); i++){

#if VERBOSE >= 1
      std::cout << "--------------------------" << std::endl;
#endif

      // increment time
      this->t++;

      // create temporary mc
      MC mc(data(i,_), this->t);

      // get distance to all other mcs
      Rcpp::NumericVector distances = this->getDistanceVector(mc, this->micro);

      // insert into closest or init new mc
      this->insert(distances, mc);

      // cleanup every tgap
      if(this->t % this->tgap == 0){
        this->cleanup();
      }


      // Incremental Reclustering part
      if(this->init){

#if VERBOSE >= 2
        std::cout << "Perform evolutionary steps" << std::endl;
#endif

        // fixed number of generations
        for(int i=0; i<incrementalGenerations; i++){
          this->evolution();
          Rcpp::checkUserInterrupt();
        }

      } else if(this->micro.size()==this->initializeAfter){
        this->initialize();
      }

#if VERBOSE >= 3
      std::cout << "Current fitness:" << std::endl;
      Rf_PrintValue(Rcpp::wrap(this->macroFitness));
#endif

    }

  }



  void cleanup(){

#if VERBOSE >= 2
    std::cout << "Cleanup" << std::endl;
#endif
    this->updateWeights();

    // merge close mcs
    for(int i = this->micro.size()-1; i >=0; i--){
      for(int j = i-1; j>=0; j--){
        double d = this->micro[i].distance(this->micro[j]); // calc distance
        if(d <= r){
#if VERBOSE >= 2
          std::cout << "Merging cluster" << i << " into " << j << std::endl;
#endif
          this->micro[j].merge(this->micro[i], this->t, this->lambda, this->r); //merge mc into the other
          this->removeMicroCluster(i); // remove the old mc
          break;
        }
      }
    }
  }

  void updateWeights(){

    // fade Clusters
    for(int i=micro.size()-1; i>=0; i--){
      this->micro[i].fade(this->t, this->lambda);
      // remove insufficient weight
      if( this->micro[i].weight <= this->omega){
        this->removeMicroCluster(i);
      }
    }
  }


  void removeMicroCluster(int i){
#if VERBOSE >= 2
    std::cout << "Remove Cluster " << i << std::endl;
#endif
    this->micro.erase(this->micro.begin()+i);
  }


  void insert(Rcpp::NumericVector distances, MC mc){

    // merge if within radius
    bool merged = 0;
    for(unsigned int i=0; i < micro.size(); i++){
      if(distances[i] <= r){
#if VERBOSE >= 2
        std::cout << "Merge observation " << t << " into Micro Cluster " << i  << std::endl;
#endif
        this->micro[i].merge(mc, this->t, this->lambda, r);
        merged=1;
      }
    }

    // if none close enough create new
    if(!merged){
#if VERBOSE >= 2
      std::cout << "Use observation " << t <<" to create new Micro Cluster" << std::endl;
#endif
      this->micro.push_back(mc);
    }
  }


  // Offline component


  void evolution(){
    if(!this->init) return;

    this->calculateFitness();

    // perform evolutionary step
    std::vector<Rcpp::NumericMatrix> selected = this->selection(); // select parents
    std::vector<Rcpp::NumericMatrix> offsprings = this->recombination(selected); // recombine parents
    std::vector<Rcpp::NumericMatrix> mutants = this->mutation(offsprings); // mutate offspring

    for(unsigned int i=0; i<mutants.size(); i++){
      double fit = this->fitness(mutants[i]); // evaluate new solution

#if VERBOSE >= 1
      std::cout << "Fitness of child: " << fit << std::endl;
#endif

      // replace weakest solution so far
      int minIdx = 0;
      double min = std::numeric_limits<double>::max();
      for(unsigned int j=0; j<this->macroFitness.size(); j++){
        if(this->macroFitness[j] < min){
          min = this->macroFitness[j];
          minIdx = j;
        }
      }

      if(min < fit){
#if VERBOSE >= 1
        std::cout << "Use new solution to replace solution " << minIdx << " with fitness " << this->macroFitness[minIdx] << std::endl;
#endif

        this->macro[minIdx] = mutants[i];
        this->macroFitness[minIdx] = fit;
      }
    }


#if VERBOSE >= 1
    std::cout << "Highest Fitness is  " <<  this->getMaxFitness(); << std::endl;
#endif

  }


  void recluster(int generations){
    if(!this->init) return;

    // fixed number of generations
    for(int i=0; i<generations; i++){
#if VERBOSE >= 1
      std::cout << "Generation " << i << std::endl;
#endif
      this->evolution();
      Rcpp::checkUserInterrupt();
    }
  }


  void calculateFitness(){

#if VERBOSE >= 3
    std::cout << "Calcualte fitness:" << std::endl;
#endif
    // for every solution
    for(unsigned int i=0; i<this->macro.size(); i++){
      this->macroFitness[i] = this->fitness(this->macro[i]);
    }
#if VERBOSE >= 3
    Rf_PrintValue(Rcpp::wrap(this->macroFitness));
#endif
  }

  double fitness(Rcpp::NumericMatrix centres){
    double result=0.0;

    Rcpp::IntegerVector assignment = this->getAssignment(centres);

    // calc distance to centers
    for(int i=0; i<assignment.size(); i++){
      result += pow(this->micro[i].distance(centres(assignment[i],_)),2) * this->micro[i].weight;
    }

    return(1/result);
  }


  void initialize(){
#if VERBOSE >= 2
    std::cout << "Initialize Solutions" << std::endl;
#endif
    this->initTime = this->t;

    // for entire population
    for(unsigned int i=0; i<this->populationSize; i++){
      //init
      macro.push_back(Rcpp::NumericMatrix(this->k, this->ndimensions()));

      // choose k centers
      Rcpp::IntegerVector x = randomShuffle(Rcpp::seq(0, this->micro.size()-1)); // Todo: this is potentially large, depending on when called
      for(unsigned int j=0; j<this->k; j++){
        int l = j % this->micro.size();
        macro[i](j,_) = this->micro[x[l]].getCentroid();
      }
    }

    this->init=1;
  }

  std::vector<Rcpp::NumericMatrix> selection(){

    // declare return value before rngscope http://gallery.rcpp.org/articles/random-number-generation/
    std::vector<Rcpp::NumericMatrix> individuals;
    individuals.reserve(2);

    RNGScope rngScope;

    // Select proportionally to fitness
    double sum=0.0;
    Rcpp::NumericVector probability(this->macroFitness.size());
    for(unsigned int i=0; i<this->macroFitness.size(); i++){
      sum += this->macroFitness[i];
      probability[i] = this->macroFitness[i];
    }

    // sample two parents
    Rcpp::IntegerVector selected(2);
    for(int i=0; i<2; i++){
      double rand = R::runif(0,1);

      for(int j=0; j<probability.size(); j++){

        double val = probability(j)/sum;

        if(val > rand){
          selected(i)=j;
          sum -= probability(j);
          probability.erase(j);
          break;
        }
        rand -= val;
      }
    }

    // since we deleted one element
    if(selected(1) >= selected(0)){
      selected(1)++;
    }

#if VERBOSE >= 1
    std::cout << "Select " << selected(0) << " and " << selected(1) << " with fitness " << this->macroFitness(selected(0)) << " and " << this->macroFitness(selected(1)) << " as parents" << std::endl;
#endif

    individuals.push_back(clone(this->macro[selected(0)]));
    individuals.push_back(clone(this->macro[selected(1)]));

#if VERBOSE >= 3
    std::cout << "Parent1:" << individuals[0] << std::endl;
    std::cout << "Parent2:" << individuals[1] << std::endl;
#endif

    return(individuals);
  }

  // wrapper for easier change of recombination algorithm
  std::vector<Rcpp::NumericMatrix> recombination(std::vector<Rcpp::NumericMatrix> individuals){
    return(recombinationGAclust(individuals));
  }

  // wrapper for easier change of mutation algorithm
  std::vector<Rcpp::NumericMatrix> mutation(std::vector<NumericMatrix> individuals){
    return(mutationGAclust(individuals));
  }

  // GA Clustering recombination approach
  std::vector<Rcpp::NumericMatrix> recombinationGAclust(std::vector<Rcpp::NumericMatrix> individuals){
    RNGScope rngScope;

    if(R::runif(0,1) < this->crossoverRate){
      int size =individuals[0].nrow()*individuals[0].ncol();
      int crossoverPoint = R::runif(0,1)*(size-1); // range 1 - length-1
#if VERBOSE >= 2
      std::cout << "CrossoverPoint: " << crossoverPoint << std::endl;
#endif

      for(int pos=0; pos < size; pos++){
        if(pos>crossoverPoint){
          double temp = individuals[0][pos];
          individuals[0][pos] = individuals[1][pos];
          individuals[1][pos] = temp;
        }
      }
    }

#if VERBOSE >= 3
    std::cout << "Offsprpring1:" << individuals[0] << std::endl;
    std::cout << "Offsprpring2:" << individuals[1] << std::endl;
#endif

    return(individuals);
  }

  // PESAII reclustering approach
  std::vector<Rcpp::NumericMatrix> recombinationPESAII(std::vector<Rcpp::NumericMatrix> individuals){

    RNGScope rngScope;

    for(unsigned int i=0; i<individuals.size(); i++){
      int size =individuals[0].nrow()*individuals[0].ncol();
      for(int pos=0; pos < size; pos++){
        if(R::runif(0,1) < this->crossoverRate){
          double temp = individuals[0][pos];
          individuals[0][pos] = individuals[1][pos];
          individuals[1][pos] = temp;
        }
      }
    }

#if VERBOSE >= 3
    std::cout << "Offsprpring1:" << individuals[0] << std::endl;
    std::cout << "Offsprpring2:" << individuals[1] << std::endl;
#endif

    return(individuals);
  }


  // GA Clustering mutation approach
  std::vector<Rcpp::NumericMatrix> mutationGAclust(std::vector<NumericMatrix> individuals){

    RNGScope rngScope;

    // for both individuals
    for(unsigned int i=0; i<individuals.size(); i++){
      int size=individuals[i].nrow() * individuals[i].ncol();
      for(int pos=0; pos < size; pos++){
        if(R::runif(0,1) < this->mutationRate){
          double val=0.0;

          if(individuals[i][pos]!=0){
            val = 2 * R::runif(0,1) * individuals[i][pos];
          } else{
            val = 2 * R::runif(0,1);
          }

          // + or - with equal change
          if(R::runif(0,1)>0.5){
            individuals[i][pos] += val;
          } else{
            individuals[i][pos] -= val;
          }
        }
      }
    }

#if VERBOSE >= 3
    std::cout << "Mutant1:" << individuals[0] << std::endl;
    std::cout << "Mutant2:" << individuals[1] << std::endl;
#endif

    return(individuals);
  }

  // PESAII mutation approach
  std::vector<Rcpp::NumericMatrix> mutationPESAII(std::vector<NumericMatrix> individuals){

    RNGScope rngScope;

    // for both individuals
    for(unsigned int i=0; i<individuals.size(); i++){
      int size=individuals[i].nrow() * individuals[i].ncol();
      for(int pos=0; pos < size; pos++){
        if(R::runif(0,1) < this->mutationRate){
          individuals[i][pos] += R::rnorm(0,.3);
        }
      }
    }

#if VERBOSE >= 3
    std::cout << "Mutant1:" << individuals[0] << std::endl;
    std::cout << "Mutant2:" << individuals[1] << std::endl;
#endif

    return(individuals);
  }


  // Helper

  Rcpp::IntegerVector getAssignment(Rcpp::NumericMatrix centres){
    Rcpp::IntegerVector assignment(this->micro.size());
    // for every cluster
    for(unsigned int i=0; i<this->micro.size(); i++){
      double min=std::numeric_limits<double>::max();
      // find closest centre
      for(int j=0; j<centres.nrow(); j++){
        double dist = this->micro[i].distance(centres(j,_));
        if(dist < min){
          min=dist;
          assignment(i)=j;
        }
      }
    }
    return(assignment);
  }


  double getMaxFitness(){
    // find max fitness
    if(!this->init){
      return(0);
    }

    double max=-1*std::numeric_limits<double>::max();
    for(unsigned int i=0; i<this->macroFitness.size(); i++){
      if(this->macroFitness[i]>max){
        max=this->macroFitness[i];
      }
    }
    return(max);
  }


  int ndimensions(){
    if(!micro.size()){
      return(0);
    } else{
      return(this->micro[0].getCentroid().size());
    }
  }


  int sampleProportionally(Rcpp::NumericVector data){

    // declare return value before rngScope http://gallery.rcpp.org/articles/random-number-generation/
    int j=0;
    RNGScope rngScope;

    // sample proportionally to distance
    double cumsum=0.0;
    for(j=0; j<data.size(); j++){
      cumsum += data(j);
      if(cumsum >= R::runif(0,1)){
        return(j);
      }
    }
    return(-1);
  }


  Rcpp::NumericVector getDistanceVector(MC mc, std::vector<MC> cluster){

    Rcpp::NumericVector distances(cluster.size());
    for(unsigned int i=0; i < cluster.size(); i++){
      distances(i) = mc.distance(cluster[i]);
    }
    return(distances);
  }


  // http://gallery.rcpp.org/articles/stl-random-shuffle/
  Rcpp::IntegerVector randomShuffle(Rcpp::IntegerVector a) {
    // clone a into b to leave a alone
    Rcpp::IntegerVector b = Rcpp::clone(a);

    // declare return value before rngscope http://gallery.rcpp.org/articles/random-number-generation/
    RNGScope scope;

    // C11 random_shuffle is now shuffle - MFH
    //std::random_shuffle(b.begin(), b.end(), randWrapper);
    std::random_device rd;
    std::mt19937 g(rd());
    std::shuffle(b.begin(), b.end(), rd);

    return(b);
  }


};


// Allows to return class objects to R
RCPP_EXPOSED_CLASS(EvoStream)
  RCPP_EXPOSED_CLASS(MC)

  // Expose class members and methods to R
  RCPP_MODULE(MOD_evoStream){
    using namespace Rcpp;

    class_<EvoStream>("EvoStream")
      .constructor()

    .method("setFields", &EvoStream::setFields)
    .method("get_microclusters", &EvoStream::get_microclusters)
    .method("get_microweights", &EvoStream::get_microweights)
    .method("get_macroclusters", &EvoStream::get_macroclusters)
    .method("get_macroweights", &EvoStream::get_macroweights)
    .method("microToMacro", &EvoStream::microToMacro)
    .method("cluster", &EvoStream::cluster)
    .method("recluster", &EvoStream::recluster)
    .method("reclusterInitialize", &EvoStream::reclusterInitialize)
    ;

    class_<MC>("MC")
      .constructor<Rcpp::NumericVector, int, double>()
      .constructor<Rcpp::NumericVector, int>()
      .field("centroid", &MC::centroid)
      .field("lastUpdate", &MC::lastUpdate)
      .field("weight", &MC::weight)

    .method("merge", &MC::merge)
    .method("fade", &MC::fade)
    ;

  }
