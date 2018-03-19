#ifndef LATENTORDERLIKELIHOOD_H_
#define LATENTORDERLIKELIHOOD_H_

#include "Model.h"
#include "ShallowCopyable.h"
#include "Ranker.h"

#include <cmath>
#include <Rcpp.h>
#include <assert.h>
#include <vector>

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
	VectorPtr order;

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

	void generateOrder(std::vector<int>& vertexOrder){
		vertexOrder.resize(order->size());
		std::vector<int> y(vertexOrder.size());
		//get ranks. ties broken randomly
		rank(*order, y, "random");

		//get ordered indices of ranks
		for(int i=0;i<y.size();i++)
			vertexOrder[i] = i;
		std::sort(  std::begin(vertexOrder),
		                std::end(vertexOrder), IdxCompare(y));

		//for(int i=0;i<vertexOrder.size();i++){
		//	std::cout << vertexOrder.at(i) << " " << order->at(vertexOrder.at(i)) << "\n";
		//}
	}


	void removeEdges(ModelPtr mod){
		/*boost::shared_ptr< std::vector<std::pair<int,int> > > edgelist = mod->network()->edgelist();
		long n = edgelist->size();
		for(int i=0;i<n;i++){
			mod->network()->removeEdge(edgelist->at(i).first,edgelist->at(i).second);
		}
		mod->calculate();*/
		mod->network()->emptyGraph();
	}
public:

	LatentOrderLikelihood(){}

	LatentOrderLikelihood(Model<Engine> mod){
		model = mod.clone();
		noTieModel = mod.clone();
		noTieModel->setNetwork(mod.network()->clone());
		removeEdges(noTieModel);
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

	void setOrder(std::vector<int>& newOrder){
		order = VectorPtr(new std::vector<int>(newOrder));
	}

	std::vector<int> getOrder(){
		return *order;
	}

	ModelPtr getModel(){
		return model;
	}


	/*!
	 * Get model exposed to R
	 */
	SEXP getModelR(){
		return wrap(*model);
	}

	List variationalModelFrame(int nOrders, double downsampleRate){
		List result;

		long n = model->network()->size();
		for(int i=0; i<nOrders; i++){
			std::vector<int> vertices(n);
			if(order){
				//std::cout << order->size() << "generating network from order" << order->size() << "\n" ;
				this->generateOrder(vertices);
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
		//std::cout << "enter conditionalLogLik\n";
		GetRNGstate();
		long n = model->network()->size();
		long nStats = model->thetas().size();

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
		bool hasEdge;
		double lpartition = 0.0;
		for(int i=0; i < n; i++){
			int vertex = workingVertOrder[i];
			this->shuffle(workingVertOrder,i);
			//std::cout <<"(" << i << ")\n";
			for(int j=0; j < i; j++){
				int alter = workingVertOrder[j];
				sample = Rf_runif(0.0,1.0) < downsampleRate;
				//std::cout <<"(" << vertex << ", " << alter << ")\n";
				if(runningModel->network()->hasEdge(vertex,alter)){
					Rf_error("Logic error: edge found where there should be none.\n");
				}
				bool hasEdge = model->network()->hasEdge(vertex, alter);
				if(sample){
					//std::cout <<"sample: (" << vertex << ", " << alter << ")\n";
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

/*	List conditionalLogLik(double downsampleRate, std::vector<int> vert_order){
		//std::cout << "enter conditionalLogLik\n";
		GetRNGstate();
		long n = model->network()->size();
		long nStats = model->thetas().size();
		std::vector<long> vertices(n);
		std::vector<std::pair<long, long> > dyad_order;
		for(int i=0; i<n;i++){
			//long ind = floor(Rf_runif(0.0,1.0)*n);
			vertices[i] = i;
			for(int j=i+1;j<n;j++){
				dyad_order.push_back(std::make_pair(i,j));
			}
		}
		this->shuffle(dyad_order,dyad_order.size());
		//this->shuffle(vertices, n);

		ModelPtr runningModel = noTieModel->clone();
		runningModel->setNetwork(noTieModel->network()->clone());
		runningModel->calculate();
		//std::cout << "n2 edges: " << noTieModel->network()->nEdges();
		List samples;
		double llik = runningModel->logLik();
		double llikChange, ldenom, probTie;
		std::vector<double> terms = runningModel->statistics();
		std::vector<double>  newTerms = runningModel->statistics();
		std::vector<double> deriv(nStats, 0.0);
		NumericMatrix hessian(nStats, nStats);
		for(int k=0; k < nStats; k++){
			for(int l=0; l < nStats; l++){
				hessian(k,l) = 0.0;
			}
		}
		bool sample;
		double lpartition = 0.0;
		for(int i=0; i < n; i++){
			int vertex = vert_order[i];
			this->shuffle(vert_order,i);

			for(int j=0; j < i; j++){
				int alter = vert_order[j];
				sample = Rf_runif(0.0,1.0) < downsampleRate;
				runningModel->statistics(terms);
				//std::cout <<"(" << vertex << ", " << alter << ")\n";
				if(runningModel->network()->hasEdge(vertex,alter)){
					Rf_error("Logic error: edge found where there should be none.\n");
				}
				llik = runningModel->logLik();
				//Calculate likelihood for vertex --> alter
				runningModel->dyadUpdate(vertex, alter, vert_order, i);
				runningModel->network()->toggle(vertex, alter);
				llikChange = runningModel->logLik() - llik;
				//std::cout << llikChange;
				ldenom = R::log1pexp(llikChange);//log(1.0 + exp(llikChange));
				lpartition += ldenom;
				probTie = exp(llikChange - ldenom);
				runningModel->statistics(newTerms);
				bool hasEdge = model->network()->hasEdge(vertex, alter);
				for(int k=0; k < nStats; k++){
					double changeK = newTerms[k] - terms[k];
					deriv[k] += changeK * (hasEdge - probTie);  //(hasEdge ? changeK : 0.0) - probTie * changeK;
					for(int l=0; l < nStats; l++){
						double changeL = newTerms[l] - terms[l];
						hessian(k,l) -= changeK * changeL * probTie * (1.0 - probTie);
					}
				}

				if(hasEdge){
					llik += llikChange;
				}else{
					runningModel->dyadUpdate(vertex, alter, vert_order, i);
					runningModel->network()->toggle(vertex, alter);
				}

				if(sample){
					for(int k=0; k<terms.size(); k++){
						terms[k] = newTerms[k] - terms[k];
					}
					List s;
					s["hasEdge"] = wrap(model->network()->hasEdge(vertex, alter));
					s["changeStats"] = wrap(terms);
					samples.push_back(s);
				}

				if(runningModel->network()->isDirected()){
					//Calculate likelihood for alter --> vertex
					sample = Rf_runif(0.0,1.0) < downsampleRate;
					runningModel->statistics(terms);

					runningModel->dyadUpdate(alter, vertex, vert_order, i);
					runningModel->network()->toggle(alter, vertex);
					llikChange = runningModel->logLik() - llik;
					ldenom = R::log1pexp(llikChange);//log(1.0 + exp(llikChange));
					lpartition += ldenom;
					probTie = exp(llikChange - ldenom);
					runningModel->statistics(newTerms);
					bool hasEdge = model->network()->hasEdge(alter, vertex);
					for(int k=0; k < nStats; k++){
						double changeK = newTerms[k] - terms[k];
						deriv[k] += (hasEdge ? changeK : 0.0) - probTie * changeK;
						for(int l=0; l < nStats; l++){
							double changeL = newTerms[l] - terms[l];
							hessian(k,l) -= changeK * changeL * probTie * (1.0 - probTie);
						}
					}
					if(hasEdge){
						llik += llikChange;
					}else{
						runningModel->dyadUpdate(alter, vertex, vert_order, i);
						runningModel->network()->toggle(alter, vertex);
					}

					if(sample){
						for(int k=0; k<terms.size(); k++){
							terms[k] = newTerms[k] - terms[k];
						}
						List s;
						s["hasEdge"] = wrap(model->network()->hasEdge(alter, vertex));
						s["changeStats"] = wrap(terms);
						samples.push_back(s);
					}
				}


			}
		}
		//runningModel->calculate();
		//newTerms = runningModel->statistics();
		//for(int k=0; k < nStats; k++){
		//	deriv[k] = newTerms[k] - deriv[k];
		//}
		//std::cout << "db:" << newTerms[0];
		PutRNGstate();
		List result;
		result["logLik"] = llik;
		result["logPartition"] = lpartition;
		result["derivative"] = wrap(deriv);
		result["hessian"] = hessian;
		result["samples"] = samples;
		return result;
	}*/

	SEXP generateNetwork(){
		GetRNGstate();
		long n = model->network()->size();
		std::vector<int> vertices(n);
		if(order){
			//std::cout << order->size() << "generating network from order" << order->size() << "\n" ;
			this->generateOrder(vertices);
		}else{
			for(int i=0; i<n;i++){
				vertices[i] = i;
			}
			this->shuffle(vertices, n);
		}
		PutRNGstate();
		return this->generateNetworkWithOrder(vertices);
		//return this->generateNetworkRandomDyad();
	}



	SEXP generateNetworkWithOrder(std::vector<int> vert_order){
		//std::cout << "enter conditionalLogLik\n";
		GetRNGstate();
		long n = model->network()->size();
		/*std::vector<long> vertices(n);
		for(int i=0; i<n;i++){
			//long ind = floor(Rf_runif(0.0,1.0)*n);
			vertices[i] = i;
		}
		this->shuffle(vertices, n);
		std::vector<long> vert_order = vertices;*/
		long nStats = model->thetas().size();

		//The model used for generating the network draw
		ModelPtr runningModel = noTieModel->clone();
		runningModel->setNetwork(noTieModel->network()->clone());
		runningModel->calculate();


		std::vector<double> eStats = std::vector<double>(nStats, 0.0);//runningModel->statistics();
		std::vector<double> stats = std::vector<double>(nStats, 0.0);
		std::vector<double> auxStats = std::vector<double>(nStats, 0.0);
		//std::vector<double> obsStats = std::vector<double>(nStats, 0.0);
		std::vector<double> terms = runningModel->statistics();
		std::vector<double>  newTerms = runningModel->statistics();
		std::vector<double>  emptyStats = runningModel->statistics();

		std::vector<int> workingVertOrder = vert_order;

		/*NumericMatrix sumCov(nStats, nStats);
		for(int k=0; k < nStats; k++){
			for(int l=0; l < nStats; l++){
				sumCov(k,l) = 0.0;
			}
		}*/
		bool directedGraph = runningModel->network()->isDirected();
		//std::cout << "n2 edges: " << noTieModel->network()->nEdges();
		double llik = runningModel->logLik();
		double llikChange, ldenom, probTie;
		bool hasEdge = false;
		//std::cout << "\n";
		for(int i=0; i < n; i++){
			int vertex = workingVertOrder[i];
			this->shuffle(workingVertOrder,i);
			//std::cout  << "\n"<< vertex << "      ";
			for(int j=0; j < i; j++){
				int alter = workingVertOrder[j];
				//std::cout << alter << " ";
				//update the observed network statistics
				/*if(model->network()->hasEdge(vertex, alter)){
					obsRunningModel->statistics(terms);
					obsRunningModel->dyadUpdate(vertex, alter);
					obsRunningModel->network()->toggle(vertex, alter);
					obsRunningModel->statistics(newTerms);
					for(int m=0; m<terms.size(); m++){
						obsStats[m] += newTerms[m] - terms[m];
					}
				}*/

				//std::cout <<"(" << vertex << ", " << alter << ")\n";
				//if(runningModel->network()->hasEdge(vertex,alter)){
				//	Rf_error("Logic error: edge found where there should be none.\n");
				//}
				llik = runningModel->logLik();
				//runningModel->statistics(terms);
				runningModel->dyadUpdate(vertex, alter, vert_order, i);
				//runningModel->network()->toggle(vertex, alter);
				runningModel->statistics(newTerms);
				llikChange = runningModel->logLik() - llik;
				//std::cout << llikChange;
				//ldenom = R::log1pexp(llikChange);
				//probTie = exp(llikChange - ldenom);
				//ldenom = log(1.0 + exp(llikChange));
				probTie = 1.0 / (1.0 + exp(-llikChange));
				//std::cout << probTie << "\n";
				hasEdge = false;
				if(Rf_runif(0.0, 1.0) < probTie){
					//runningModel->dyadUpdate(vertex, alter, vert_order, i);
					runningModel->network()->toggle(vertex, alter);
					hasEdge = true;
				}else
					runningModel->rollback();
				//std::cout << runningModel->network()->nEdges()<<"\n";
				/*for(int k=0; k < nStats; k++){
					double changeK = newTerms[k] - terms[k];
					for(int l=0; l < nStats; l++){
						double changeL = newTerms[l] - terms[l];
						sumCov(k,l) += changeK * changeL * probTie * (1.0 - probTie) ;
					}
				}*/


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
					/*
					if(model->network()->hasEdge(alter, vertex)){
						obsRunningModel->statistics(terms);
						obsRunningModel->dyadUpdate(alter, vertex);
						obsRunningModel->network()->toggle(alter, vertex);
						obsRunningModel->statistics(newTerms);
						for(int m=0; m<terms.size(); m++){
							obsStats[m] += newTerms[m] - terms[m];
						}
					}
					*/

					if(runningModel->network()->hasEdge(alter, vertex)){
						Rf_error("Logic error: edge found where there should be none.\n");
					}
					llik = runningModel->logLik();
					//runningModel->statistics(terms);
					runningModel->dyadUpdate(alter, vertex, vert_order, i);
					//runningModel->network()->toggle(alter, vertex);
					runningModel->statistics(newTerms);
					llikChange = runningModel->logLik() - llik;
					//std::cout << llikChange;
					//ldenom = R::log1pexp(llikChange);//log(1.0 + exp(llikChange));
					//probTie = exp(llikChange - ldenom);
					probTie = 1.0 / (1.0 + exp(-llikChange));
					//std::cout << probTie << "\n";
					hasEdge=false;
					if(Rf_runif(0.0, 1.0) < probTie){
						//runningModel->dyadUpdate(alter, vertex, vert_order, i);
						runningModel->network()->toggle(alter, vertex);
						hasEdge=true;
					}else
						runningModel->rollback();

					/*
					for(int k=0; k < nStats; k++){
						double changeK = newTerms[k] - terms[k];
						for(int l=0; l < nStats; l++){
							double changeL = newTerms[l] - terms[l];
							double actStat = hasEdge ? changeL : 0.0;
							grad(k,l) -= changeK * changeL * probTie * (1.0 - probTie) +
									changeK * probTie * (stats[l] - eStats[l]);
						}
					}
					*/
					/*for(int k=0; k < nStats; k++){
						double changeK = newTerms[k] - terms[k];
						for(int l=0; l < nStats; l++){
							double changeL = newTerms[l] - terms[l];
							sumCov(k,l) -= changeK * changeL * probTie * (1.0 - probTie) ;
						}
					}*/
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
		DiscreteAttrib attr = DiscreteAttrib();
		attr.setName("__order__");
		runningModel->network()->addDiscreteVariable(vert_order, attr);
		PutRNGstate();
		List result;
		result["network"] = runningModel->network()->cloneR();
		result["emptyNetworkStats"] = wrap(emptyStats);
		result["stats"] = wrap(stats);
		result["expectedStats"] = wrap(eStats);
		//result["observedStats"] = wrap(obsStats);

		//result["sumCov"] = sumCov;
		//result["gradient1"] = grad;
		//result["gradient2"] = grad2;
		return result;
	}
};


}
#endif /* LATENTORDERLIKELIHOOD_H_ */
