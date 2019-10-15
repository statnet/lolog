#ifndef LATENTORDERLIKELIHOOD_H_
#define LATENTORDERLIKELIHOOD_H_

#include "Model.h"
#include "ShallowCopyable.h"
#include "Ranker.h"

#include <cmath>
#include <Rcpp.h>
#include <assert.h>
#include <vector>
#include <iterator>

namespace lolog{


struct IdxCompare
{
  const std::vector<int>& target;
  
  IdxCompare(const std::vector<int>& target): target(target) {}
  
  bool operator()(int a, int b) const { return target[a] < target[b]; }
};

template<class Engine>
class LatentOrderLikelihood : public ShallowCopyable{
protected:
  typedef boost::shared_ptr< Model<Engine> > ModelPtr;
  typedef boost::shared_ptr< std::vector<int> > VectorPtr;
  
  /**
  * The likelihood model with the observed graph
  */
  ModelPtr model;
  
  /**
  * The likelihood model with an empty graph
  */
  ModelPtr noTieModel;
  
  /**
  * A vector giving the (partial) ordering for vertex inclusion
  */
  //VectorPtr order;
  
  /**
  * Fisher-Yates shuffle of elements up to offset
  */
  template<class T>
  void shuffle(std::vector<T>& vec, long offset){
    for( int i=0; i < offset - 1.0; i++){
      //long ind = floor(Rf_runif(0.0,1.0)*offset);
      long ind = floor(Rf_runif(i,offset));
      T tmp = vec[i];
      vec[i] = vec[ind];
      vec[ind] = tmp;
    }
  }
  
  /**
  * Generates a vertex ordering 'vertexOrder' conditional upon a possibly
  * partial ordering 'order'.
  */
  void generateOrder(std::vector<int>& vertexOrder,const VectorPtr order){
    vertexOrder.resize(order->size());
    std::vector<int> y(vertexOrder.size());
    //get ranks. ties broken randomly
    rank(*order, y, "random");
    
    //get ordered indices of ranks
    for(int i=0;i<y.size();i++)
      vertexOrder[i] = i;
    std::sort(  vertexOrder.begin(),
                vertexOrder.end(), IdxCompare(y));
    
  }
  
  
  void removeEdges(ModelPtr mod){
    mod->network()->emptyGraph();
  }
public:
  
  LatentOrderLikelihood(){}
  
  LatentOrderLikelihood(Model<Engine> mod){
    model = mod.clone();
    noTieModel = mod.clone();
    noTieModel->setNetwork(mod.network()->clone());
    removeEdges(noTieModel);
    if(model->hasVertexOrder() && model->getVertexOrder()->size() != model->network()->size())
      Rf_error("Vertex ordering does not have the same number of elements as there are vertices in the network 95.");
  }
  
  /*!
  * R constructor for RCPP
  *
  */
  LatentOrderLikelihood(SEXP sexp){
    boost::shared_ptr<LatentOrderLikelihood> xp = unwrapRobject< LatentOrderLikelihood<Engine> >(sexp);
    model = xp->model;
    noTieModel = xp->noTieModel;
    order = xp->order;
  }
  
  /*!
  * coerce to R object. for RCPP
  */
  operator SEXP() const{
    return wrapInReferenceClass(*this,Engine::engineName() + "LatentOrderLikelihood");
  }
  
  virtual ShallowCopyable* vShallowCopyUnsafe() const{
    return new LatentOrderLikelihood(*this);
  }
  
  ~LatentOrderLikelihood(){}
  
  void setModel(const Model<Engine>& mod){
    model = mod.clone();
    noTieModel = mod.clone();
    noTieModel->setNetwork(mod.network()->clone());
    removeEdges(noTieModel);
    noTieModel->calculate();
  }
  
  
  void setThetas(std::vector<double> newThetas){
    model->setThetas(newThetas);
    noTieModel->setThetas(newThetas);
  }
  
  ModelPtr getModel(){
    return model;
  }
  
  
  /*!
  * Get model exposed to R
  */
  Rcpp::RObject getModelR(){
    return wrap(*model);
  }
  
  List variationalModelFrame(int nOrders, double downsampleRate){
    List result;
    
    long n = model->network()->size();
    for(int i=0; i<nOrders; i++){
      std::vector<int> vertices(n);
      if(model->hasVertexOrder()){
        this->generateOrder(vertices, model->getVertexOrder());
      }else{
        for(int i=0; i<n;i++){
          vertices[i] = i;
        }
        this->shuffle(vertices, n);
      }
      result.push_back(this->modelFrameGivenOrder(downsampleRate, vertices));
    }
    return result;
  }
  
  
  List variationalModelFrameWithFunc(int nOrders, double downsampleRate, Function vertexOrderingFunction){
    List result;
    for( int i=0; i<nOrders; i++){
      GetRNGstate();
      std::vector<int> vertices = as< std::vector<int> >(vertexOrderingFunction());
      PutRNGstate();
      result.push_back(this->modelFrameGivenOrder(downsampleRate, vertices));
    }
    return result;
  }
  
  List modelFrameGivenOrder(double downsampleRate, std::vector<int> vert_order){
    GetRNGstate();
    long n = model->network()->size();
    //long nStats = model->thetas().size();
    
    ModelPtr runningModel = noTieModel->clone();
    runningModel->setNetwork(noTieModel->network()->clone());
    runningModel->calculate();
    List samples;
    std::vector<double> terms = runningModel->statistics();
    std::vector<double>  newTerms = runningModel->statistics();
    
    std::vector<int> workingVertOrder = vert_order;
    
    std::vector<int> outcome;
    std::vector< std::vector<double> > predictors(terms.size());
    for(int i=0;i<predictors.size();i++){
      predictors.at(i).reserve(floor(downsampleRate * noTieModel->network()->maxEdges()) + 1000);
    }
    
    bool sample;
    //bool hasEdge;
    //double lpartition = 0.0;
    for(int i=0; i < n; i++){
      int vertex = workingVertOrder[i];
      this->shuffle(workingVertOrder,i);
      for(int j=0; j < i; j++){
        int alter = workingVertOrder[j];
        sample = Rf_runif(0.0,1.0) < downsampleRate;
        assert(!runningModel->network()->hasEdge(vertex, alter));
        bool hasEdge = model->network()->hasEdge(vertex, alter);
        if(sample){
          runningModel->statistics(terms);
          runningModel->dyadUpdate(vertex, alter, vert_order, i);
          runningModel->statistics(newTerms);
          
          if(hasEdge){
            runningModel->network()->toggle(vertex, alter);
          }else{
            runningModel->rollback();
          }
          outcome.push_back(hasEdge);
          for(int k=0; k<terms.size(); k++){
            predictors[k].push_back(newTerms[k] - terms[k]);
          }
        }else{
          if(hasEdge){
            runningModel->dyadUpdate(vertex, alter, vert_order, i);
            runningModel->network()->toggle(vertex, alter);
          }
        }
        
        if(runningModel->network()->isDirected()){
          hasEdge = model->network()->hasEdge(alter, vertex);
          if(sample){
            runningModel->statistics(terms);
            runningModel->dyadUpdate(alter, vertex, vert_order, i);
            runningModel->statistics(newTerms);
            
            if(hasEdge){
              runningModel->network()->toggle(alter, vertex);
            }else{
              runningModel->rollback();
            }
            outcome.push_back(hasEdge);
            for(int k=0; k<terms.size(); k++){
              predictors[k].push_back(newTerms[k] - terms[k]);
            }
          }else{
            if(hasEdge){
              runningModel->dyadUpdate(alter, vertex, vert_order, i);
              runningModel->network()->toggle(alter, vertex);
            }
          }
        }
        
        
      }
    }
    
    PutRNGstate();
    List result;
    result["outcome"] = wrap(outcome);
    result["samples"] = wrap(predictors);
    return result;
  }
  
  Rcpp::RObject generateNetwork(){
    GetRNGstate();
    long n = model->network()->size();
    std::vector<int> vertices(n);
    if(model->hasVertexOrder()){
      this->generateOrder(vertices, model->getVertexOrder());
    }else{
      for(int i=0; i<n;i++){
        vertices[i] = i;
      }
      this->shuffle(vertices, n);
    }
    PutRNGstate();
    return this->generateNetworkWithOrder(vertices);
  }
  
  
  
  Rcpp::RObject generateNetworkWithOrder(std::vector<int> vert_order){
    GetRNGstate();
    long n = model->network()->size();
    long nStats = model->thetas().size();
    
    //The model used for generating the network draw
    ModelPtr runningModel = noTieModel->clone();
    runningModel->setNetwork(noTieModel->network()->clone());
    runningModel->calculate();
    
    
    std::vector<double> eStats = std::vector<double>(nStats, 0.0);//runningModel->statistics();
    std::vector<double> stats = std::vector<double>(nStats, 0.0);
    std::vector<double> auxStats = std::vector<double>(nStats, 0.0);
    std::vector<double> terms = runningModel->statistics();
    std::vector<double>  newTerms = runningModel->statistics();
    std::vector<double>  emptyStats = runningModel->statistics();
    
    std::vector<int> workingVertOrder = vert_order;
    
    bool directedGraph = runningModel->network()->isDirected();
    double llik = runningModel->logLik();
    double llikChange, probTie;//, ldenom;
    bool hasEdge = false;
    for(int i=0; i < n; i++){
      int vertex = workingVertOrder[i];
      this->shuffle(workingVertOrder,i);
      for(int j=0; j < i; j++){
        int alter = workingVertOrder[j];
        assert(!runningModel->network()->hasEdge(vertex, alter));
        llik = runningModel->logLik();
        runningModel->dyadUpdate(vertex, alter, vert_order, i);
        runningModel->statistics(newTerms);
        llikChange = runningModel->logLik() - llik;
        probTie = 1.0 / (1.0 + exp(-llikChange));
        hasEdge = false;
        if(Rf_runif(0.0, 1.0) < probTie){
          runningModel->network()->toggle(vertex, alter);
          hasEdge = true;
        }else
          runningModel->rollback();
        
        //update the generated network statistics and expected statistics
        for(int m=0; m<terms.size(); m++){
          double diff = newTerms[m] - terms[m];\
          eStats[m] += diff * probTie;
          if(hasEdge){
            stats[m] += diff;
            terms[m] += diff;
          }
        }
        if(directedGraph){
          assert(!runningModel->network()->hasEdge(alter, vertex));
          llik = runningModel->logLik();
          runningModel->dyadUpdate(alter, vertex, vert_order, i);
          runningModel->statistics(newTerms);
          llikChange = runningModel->logLik() - llik;
          probTie = 1.0 / (1.0 + exp(-llikChange));
          hasEdge=false;
          if(Rf_runif(0.0, 1.0) < probTie){
            runningModel->network()->toggle(alter, vertex);
            hasEdge=true;
          }else
            runningModel->rollback();
          
          for(int m=0; m<terms.size(); m++){
            double diff = newTerms[m] - terms[m];
            eStats[m] += diff * probTie;
            if(hasEdge){
              stats[m] += diff;
              terms[m] += diff;
            }
          }
        }
      }
    }
    std::vector<int> rankOrder = vert_order;
    for(int i=0;i<vert_order.size();i++)
      rankOrder[vert_order[i]] = i;
    DiscreteAttrib attr = DiscreteAttrib();
    attr.setName("__order__");
    runningModel->network()->addDiscreteVariable(rankOrder, attr);
    PutRNGstate();
    List result;
    result["network"] = runningModel->network()->cloneR();
    result["emptyNetworkStats"] = wrap(emptyStats);
    result["stats"] = wrap(stats);
    result["expectedStats"] = wrap(eStats);
    
    return result;
  }
  
  //Based on generate model from vertex order - generate network based on edge ordering
  Rcpp::RObject generateNetworkWithEdgeOrder(std::vector<int> perm_heads,
                                             std::vector<int> perm_tails){
    GetRNGstate();
    long n = model->network()->size();
    long nStats = model->thetas().size();
    long e = n*(n-1);
    if(!model->network()->isDirected()){
      e = e/2;
    }
    
    //throw error if the perm_heads or perm_tails are the wrong length
    if(e != perm_heads.size() || e!= perm_tails.size()){
      throw(std::range_error("Wrong length of permutation"));
    }
    
    //Make vert order that isn't used
    std::vector<int> vert_order(n);
    for(int j=0;j<n;j++){
      vert_order[j] = (j+1);
    }
    
    //The model used for generating the network draw
    ModelPtr runningModel = noTieModel->clone();
    runningModel->setNetwork(noTieModel->network()->clone());
    runningModel->calculate();
    
    
    std::vector<double> eStats = std::vector<double>(nStats, 0.0);//runningModel->statistics();
    std::vector<double> stats = std::vector<double>(nStats, 0.0);
    std::vector<double> auxStats = std::vector<double>(nStats, 0.0);
    std::vector<double> terms = runningModel->statistics();
    std::vector<double>  newTerms = runningModel->statistics();
    std::vector<double>  emptyStats = runningModel->statistics();
    
    //std::vector<int> workingVertOrder = vert_order;
    
    bool directedGraph = runningModel->network()->isDirected();
    double llik = runningModel->logLik();
    double llikChange, probTie;//, ldenom;
    bool hasEdge = false;
    for(int i=0; i < e; i++){
      int vertex = perm_tails[i];
      int alter = perm_heads[i];
      assert(!runningModel->network()->hasEdge(vertex, alter));
      llik = runningModel->logLik();
      runningModel->dyadUpdate(vertex, alter, vert_order, i);
      runningModel->statistics(newTerms);
      llikChange = runningModel->logLik() - llik;
      probTie = 1.0 / (1.0 + exp(-llikChange));
      hasEdge = false;
      if(Rf_runif(0.0, 1.0) < probTie){
        runningModel->network()->toggle(vertex, alter);
        hasEdge = true;
      }else
        runningModel->rollback();
      
      //update the generated network statistics and expected statistics
      for(int m=0; m<terms.size(); m++){
        double diff = newTerms[m] - terms[m];\
        eStats[m] += diff * probTie;
        if(hasEdge){
          stats[m] += diff;
          terms[m] += diff;
        }
      }
    }
    std::vector<int> rankOrder = vert_order;
    for(int i=0;i<vert_order.size();i++)
      rankOrder[vert_order[i]] = i;
    DiscreteAttrib attr = DiscreteAttrib();
    attr.setName("__order__");
    runningModel->network()->addDiscreteVariable(rankOrder, attr);
    PutRNGstate();
    List result;
    result["network"] = runningModel->network()->cloneR();
    result["emptyNetworkStats"] = wrap(emptyStats);
    result["stats"] = wrap(stats);
    result["expectedStats"] = wrap(eStats);
    
    return result;
  }
  
  
  //Heavily modified from the generate network function
  Rcpp::List calcChangeStats(std::vector<int> perm_heads,
                             std::vector<int> perm_tails){
    
    long n = model->network()->size();
    long nStats = model->thetas().size();
    long e = n*(n-1);
    if(!model->network()->isDirected()){
      e = e/2;
    }
    
    //Make fake vert order so don't have to change other functions
    //Will not effect calculation for order independent stats
    std::vector<int> vert_order(1,n);
    
    //Check if the perm_head and perm_tails vectors have the right number of elements
    if(perm_heads.size() != e || perm_tails.size() != e){
      Rcpp::Rcout<< "The perm is the wrong length" ;
    }
    
    //The model used for calculating the change stats
    ModelPtr runningModel = noTieModel->clone();
    runningModel->setNetwork(noTieModel->network()->clone());
    runningModel->calculate();
    
    std::vector<double> eStats = std::vector<double>(nStats, 0.0);//runningModel->statistics();
    std::vector<double> stats = std::vector<double>(nStats, 0.0);
    std::vector<double> auxStats = std::vector<double>(nStats, 0.0);
    std::vector<double> terms = runningModel->statistics();
    std::vector<double>  newTerms = runningModel->statistics();
    std::vector<double>  emptyStats = runningModel->statistics();
    Rcpp::List result(e);
    
    
    //bool directedGraph = runningModel->network()->isDirected();
    // double llik = runningModel->logLik();
    // double llikChange, probTie;//, ldenom;
    for(int i=0; i < e; i++){
      int vertex = perm_tails[i];
      int alter = perm_heads[i];
      assert(!runningModel->network()->hasEdge(vertex, alter));
      //llik = runningModel->logLik();
      std::vector<double> stat = runningModel->statistics();
      
      runningModel->dyadUpdate(vertex, alter, vert_order, vertex);
      //runningModel->statistics(newTerms);
      std::vector<double> statNew(nStats);
      runningModel->statistics(statNew);
      std::vector<double> changeStat(nStats);
      std::transform(statNew.begin(), statNew.end(),stat.begin(),changeStat.begin(),std::minus<double>());
      result[i] = changeStat;
      if(model->network()->hasEdge(vertex,alter)){
        runningModel->network()->toggle(vertex,alter);
      }else{runningModel->rollback();}
    }
    return result;
  }
  
};


}
#endif /* LATENTORDERLIKELIHOOD_H_ */
