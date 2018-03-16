/*
 * Model.h
 *
 *  Created on: Jun 27, 2011
 *      Author: ianfellows
 */

#ifndef MODELH_
#define MODELH_

#include "Stat.h"
#include "Offset.h"
#include "StatController.h"
#include <vector>
#include <string>
#include <memory>
#include <boost/shared_ptr.hpp>
#include <Rcpp.h>
#include <RcppCommon.h>
#include "ShallowCopyable.h"

namespace lolog{


/*!
 * a representation of an lolog model
 */
template<class Engine>
class Model : public ShallowCopyable{
protected:
	typedef boost::shared_ptr< AbstractStat<Engine > > StatPtr;
	typedef std::vector< StatPtr >  StatVector;

	typedef boost::shared_ptr< AbstractOffset<Engine > > OffsetPtr;
	typedef std::vector< OffsetPtr >  OffsetVector;

	StatVector stats;						//!statistics
	OffsetVector offsets;
	boost::shared_ptr< BinaryNet<Engine> > net;			//!the relevant network

	//The domain
	boost::shared_ptr< bool > randomGraph;
	boost::shared_ptr< std::vector<int> > randomDiscreteVariables;
	boost::shared_ptr< std::vector<int> > randomContinVariables;
public:
	Model(){
		//std::cout << "m1";
		boost::shared_ptr< BinaryNet<Engine> > n(new BinaryNet<Engine>());
		net=n;
		randomGraph = boost::shared_ptr< bool >(new bool);
		randomDiscreteVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
		randomContinVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
		*randomGraph = true;
	}

	Model(BinaryNet<Engine>& network){
		//std::cout << "m2";
		boost::shared_ptr< BinaryNet<Engine> > n(new BinaryNet<Engine>(network));
		net = n;
		randomGraph = boost::shared_ptr< bool >(new bool);
		randomDiscreteVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
		randomContinVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
		*randomGraph = true;
	}

	Model(const Model& mod){
		//std::cout << "m3";
		stats = mod.stats;
		offsets = mod.offsets;
		net = mod.net;
		randomGraph = mod.randomGraph;
		randomDiscreteVariables = mod.randomDiscreteVariables;
		randomContinVariables = mod.randomContinVariables;
	}

	/*!
	 * if deep, then the model statistics are de-aliased
	 */
	Model(const Model& mod, bool deep){
		//std::cout << "m4";
		stats = mod.stats;
		offsets = mod.offsets;
		net = mod.net;
		randomGraph = mod.randomGraph;
		randomDiscreteVariables = mod.randomDiscreteVariables;
		randomContinVariables = mod.randomContinVariables;
		if(deep){
			for(int i=0;i<stats.size();i++)
				stats[i] = stats[i]->vClone();
			for(int i=0;i<offsets.size();i++)
				offsets[i] = offsets[i]->vClone();
			randomGraph = boost::shared_ptr< bool >(new bool);
			randomDiscreteVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
			randomContinVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
			*randomGraph = *mod.randomGraph;
			*randomDiscreteVariables = *mod.randomDiscreteVariables;
			*randomContinVariables = *mod.randomContinVariables;
		}
	}

	virtual ~Model(){}

	/*!
	 * R constructor for RCPP
	 *
	 */
	Model(SEXP sexp){
		//std::cout << "m5";
		boost::shared_ptr<Model> xp = unwrapRobject< Model<Engine> >(sexp);
		stats = xp->stats;
		offsets = xp->offsets;
		net = xp->net;
		randomGraph = xp->randomGraph;
		randomDiscreteVariables = xp->randomDiscreteVariables;
		randomContinVariables = xp->randomContinVariables;
	}

	virtual ShallowCopyable* vShallowCopyUnsafe() const{
		return new Model(*this);
	}

	/*!
	 * coerce to R object. for RCPP
	 */
	operator SEXP() const{
		//std::cout << "mWrap";
		return wrapInReferenceClass(*this,Engine::engineName() + "Model");
	}

	/*!
	 * clone the model (but not the network assosiated with it)
	 */
	boost::shared_ptr< Model<Engine> > clone() const{
		return boost::shared_ptr< Model<Engine> >(new Model<Engine>(*this, true));
	}

	virtual boost::shared_ptr< Model<Engine> > vClone() const{
		return clone();
	}

	void copy(Model<Engine>& mod){
		stats = mod.stats;
		offsets = mod.offsets;
		net = mod.net;
		randomGraph = mod.randomGraph;
		randomDiscreteVariables = mod.randomDiscreteVariables;
		randomContinVariables = mod.randomContinVariables;
	}

	void copy(Model<Engine>& mod,bool deep){
		net = mod.net;
		if(deep){
			stats.resize(mod.stats.size());
			offsets.resize(mod.offsets->vSize());
			for(int i=0;i<stats.size();i++)
				stats[i] = mod.stats[i]->vClone();
			for(int i=0;i<offsets->vSize();i++)
				offsets[i] = mod.offsets[i]->vClone();
			*randomGraph = *mod.randomGraph;
			*randomDiscreteVariables = *mod.randomDiscreteVariables;
			*randomContinVariables = *mod.randomContinVariables;
		}else{
			stats = mod.stats;
			offsets = mod.offsets;
			randomGraph = mod.randomGraph;
			randomDiscreteVariables = mod.randomDiscreteVariables;
			randomContinVariables = mod.randomContinVariables;
		}
	}

	/*!
	 * Does the domain of the model include the graph
	 */
	bool hasRandomGraph() const{
		return *randomGraph;
	}

	void setRandomGraph(bool random){
		*randomGraph = random;
	}

	/*!
	 * Which nodal variables are included in the domain of the model
	 */
	std::vector<int> randomVariables(bool discrete) const{
		if(discrete)
			return *randomDiscreteVariables;
		else
			return *randomContinVariables;
	}

	void setRandomVariables(std::vector<int> variables, bool discrete){
		if(discrete)
			*randomDiscreteVariables = variables;
		else
			*randomContinVariables = variables;
	}

	bool hasAnyRandomVariables() const{
		return randomDiscreteVariables->size()>0 || randomContinVariables->size()>0;
	}

	void setRandomVariablesR(std::vector<std::string> vars){
		std::vector<std::string> dv = net->discreteVarNames();
		std::vector<std::string> cv = net->continVarNames();
		std::vector<int> di,ci;
		int ind;
		for(int i=0;i<vars.size();i++){
			ind = indexOf(vars[i],dv);
			if(ind>=0){
				di.push_back(ind);
			}else{
				ind = indexOf(vars[i],cv);
				if(ind>=0)
					ci.push_back(ind);
				else
					Rf_error("Model::setRandomVariables : Unknown variable");
			}
		}
		*randomDiscreteVariables = di;
		*randomContinVariables = ci;
	}

	std::vector<std::string> getRandomVariablesR() const{
		std::vector<std::string> vars;
		std::vector<std::string> dv = net->discreteVarNames();
		std::vector<std::string> cv = net->continVarNames();
		for(int i=0;i<randomDiscreteVariables->size();i++){
			vars.push_back(dv.at(randomDiscreteVariables->at(i)));
		}
		for(int i=0;i<randomContinVariables->size();i++){
			vars.push_back(cv.at(randomContinVariables->at(i)));
		}
		return vars;
	}



	/*!
	 * the model terms
	 */
	std::vector<double> terms(){
		int n=0;
		for(int i=0;i<stats.size();i++){
			n += stats.at(i)->vSize();
		}
		for(int i=0;i<offsets.size();i++){
			n += offsets.at(i)->vSize();
		}
		std::vector<double> v(n,0.0);
		int c=0;
		for(int i=0;i<stats.size();i++){
			std::vector<double> vals = stats.at(i)->vValues();
			for(int j=0;j<vals.size();j++){
				v[c] = vals[j];
				c++;
			}
		}
		for(int i=0;i<offsets.size();i++){
			std::vector<double> vals = offsets.at(i)->vValues();
			for(int j=0;j<vals.size();j++){
				v[c] = vals[j];
				c++;
			}
		}
		return v;
	}

	/*!
	 * the model parameters
	 */
	std::vector<double> thetas(){
		int n=0;
		for(int i=0;i<stats.size();i++){
			n += stats.at(i)->vTheta().size();
		}
		std::vector<double> v(n,0.0);
		int c=0;
		for(int i=0;i<stats.size();i++){
			std::vector<double> vals = stats.at(i)->vTheta();
			for(int j=0;j<vals.size();j++){
				v[c] = vals[j];
				//cout << stats.at(i)->theta()[j];
				c++;
			}
		}
		return v;
	}

	/*!
	 * set the model paramters
	 */
	void  setThetas(std::vector<double> newThetas){
		int n=0;
		for(int i=0;i<stats.size();i++){
			n += stats.at(i)->vTheta().size();
		}
		if(newThetas.size()!= n){
			//Rcpp::Rcout << n  << " " << newThetas.size() << " ";
			::Rf_error("Model.setThetas: size mismatch:");
		}
		int c=0;
		for(int i=0;i<stats.size();i++){
			std::vector<double>* vals = &stats.at(i)->vTheta();
			for(int j=0;j<vals->size();j++){
				(*vals)[j] = newThetas[c];
				//cout << stats.at(i)->theta()[j];
				c++;
			}
		}
	}


	/*!
	 * the model statistics
	 */
	std::vector<double> statistics(){
		int n=0;
		for(int i=0;i<stats.size();i++){
			n += stats.at(i)->vSize();
		}
		std::vector<double> v(n,0.0);
		int c=0;
		for(int i=0;i<stats.size();i++){
			//std::vector<double> vals = stats.at(i)->vStatistics();
			for(int j=0;j<stats.at(i)->vStatistics().size();j++){
				v[c] = stats.at(i)->vStatistics()[j];
				c++;
			}
		}
		return v;
	}
	/*!
	 * Copy model statistics into v
	 */
	void statistics(std::vector<double>& v){
			int c=0;
			for(int i=0;i<stats.size();i++){
				//std::vector<double> vals = stats.at(i)->vStatistics();
				for(int j=0;j<stats[i]->vStatistics().size();j++){
					v[c] = stats[i]->vStatistics()[j];
					c++;
				}
			}
		}

	/*!
	 * returns statistics with names for R
	 */
	NumericVector statisticsR(){
		NumericVector res = wrap(statistics());
		res.attr("names") = wrap(names());
		return res;
	}

	/*!
	 * returns thetas with names for R
	 */
	NumericVector thetasR(){
		NumericVector res = wrap(thetas());
		res.attr("names") = wrap(names());
		return res;
	}

	/*!
	 * the model statistic names
	 */
	std::vector<std::string> names(){
		int n=0;
		for(int i=0;i<stats.size();i++){
			n += stats.at(i)->vSize();
		}
		std::vector<std::string> v(n,"??");
		int c=0;
		for(int i=0;i<stats.size();i++){
			std::vector<std::string> vals = stats.at(i)->vStatNames();
			for(int j=0;j<vals.size();j++){
				v[c] = vals[j];
				c++;
			}
		}
		return v;
	}

	/*!
	 * the model offsets
	 */
	std::vector<double> offset(){
		int n=0;
		for(int i=0;i<offsets.size();i++){
			n += offsets.at(i)->vSize();
		}
		std::vector<double> v(n,0.0);
		int c=0;
		for(int i=0;i<offsets.size();i++){
			std::vector<double> vals = offsets.at(i)->vValues();
			for(int j=0;j<vals.size();j++){
				v[c] = vals[j];
				c++;
			}
		}
		return v;
	}

	/*!
	 * the log likelihood of the model
	 */
	double logLik(){
		double ll = 0.0;
		for(int i=0;i<stats.size();i++){
			ll += stats[i]->vLogLik();
		}
		for(int i=0;i<offsets.size();i++){
			ll += offsets[i]->vLogLik();
		}
		return ll;
	}

	virtual double vLogLik(){
		return logLik();
	}

	/*!
	 * add a statistic
	 */
	void addStatPtr(StatPtr  s){
		stats.push_back(s);
		s->vCalculate(*net);
	}

	/*!
	 * add a statistic to the model
	 */
	void addStat(const AbstractStat<Engine>&  s){
		StatPtr ps((&s)->clone());
		ps->vCalculate(*net);
		stats.push_back(ps);
	}

	/*!
	 * add a offset
	 */
	void addOffsetPtr(OffsetPtr  o){
		offsets.push_back(o);
		o->vCalculate(*net);
	}

	/*!
	 * add a offset to the model
	 */
	void addOff(const AbstractOffset<Engine>&  o){
		OffsetPtr ps((&o)->vClone());
		ps->vCalculate(*net);
		offsets.push_back(ps);
	}

	/*!
	 * add a statistic by name. uses StatController
	 */
	void addStatistic(const std::string name, Rcpp::List params){
		AbstractStat<Engine>* ps = StatController<Engine>::getStat(name, params);
		if(ps==NULL){
			::Rf_error("Invalid stat");
			return;
		}
		ps->vCalculate(*net);
		stats.push_back(StatPtr(ps));
	}

	/*!
	 * add a offset by name. uses StatController
	 */
	void addOffset(const std::string name, Rcpp::List params){
		AbstractOffset<Engine>* ps = StatController<Engine>::getOffset(name, params);
		if(ps==NULL){
			::Rf_error("Invalid offset");
			return;
		}
		ps->vCalculate(*net);
		offsets.push_back(OffsetPtr(ps));
	}

	/*!
	 * calculates statistics and offsets
	 */
	void calculate(){
		calculateStatistics();
		calculateOffsets();
	}

	/*!
	 * calculate the statistics
	 */
	void calculateStatistics(){
		for(int i=0;i<stats.size();i++){
			stats[i]->vCalculate(*net);
		}
	}

	/*!
	 * calculate the statistics
	 */
	void calculateOffsets(){
		for(int i=0;i<offsets.size();i++){
			offsets[i]->vCalculate(*net);
		}
	}

	void dyadUpdate(int &from, int &to, std::vector<int> &order, int &actorIndex){
		for(int k=0;k<stats.size();k++){
			stats[k]->vDyadUpdate(*net, from, to, order, actorIndex);
		}
		for(int k=0;k<offsets.size();k++){
			offsets[k]->vDyadUpdate(*net, from, to, order, actorIndex);
		}
	}


	void discreteVertexUpdate(int vertex, int variable, int newValue, std::vector<int> &order, int &actorIndex){
		for(int k=0;k<stats.size();k++)
			stats[k]->vDiscreteVertexUpdate(*net,vertex, variable, newValue);
		for(int k=0;k<offsets.size();k++)
			offsets[k]->vDiscreteVertexUpdate(*net,vertex, variable, newValue);
	}

	void continVertexUpdate(int vertex, int variable, double newValue, std::vector<int> &order, int &actorIndex){
		for(int k=0;k<stats.size();k++)
			stats[k]->vContinVertexUpdate(*net,vertex, variable, newValue, order, actorIndex);
		for(int k=0;k<offsets.size();k++)
			offsets[k]->vContinVertexUpdate(*net,vertex, variable, newValue, order, actorIndex);
	}

	void rollback(){
		for(int k=0;k<stats.size();k++)
			stats[k]->vRollback(*net);
		for(int k=0;k<offsets.size();k++)
			offsets[k]->vRollback(*net);
	}

	/*!
	 * get the network
	 */
	boost::shared_ptr< BinaryNet<Engine> > network() const{
		return net;
	}

	SEXP getNetworkR() const{
		return wrap(*net);
	}

	/*!
	 * set the network
	 */
	void setNetwork(const BinaryNet<Engine>& network){
		boost::shared_ptr< BinaryNet<Engine> > n(new BinaryNet<Engine>(network));
		net = n;
	}
	void setNetworkR(const BinaryNet<Engine>& network){
		boost::shared_ptr< BinaryNet<Engine> > n(new BinaryNet<Engine>(network));
		net = n;
	}
	void setNetwork(const boost::shared_ptr< BinaryNet<Engine> > network){
		net = network;
	}

};


/**
 * Reduced entropy
 */
template<class Engine>
class ReModel : public Model<Engine>{
protected:
	typedef boost::shared_ptr< std::vector<double> > ParamPtr;
	typedef boost::shared_ptr< bool > BoolPtr;
	ParamPtr betas;		// coefficient for penalties
	ParamPtr centers;	// The mean values to center the penalty on beta * (center - g(x))^2
	BoolPtr isThetaDep;				// ( theta / beta)^2 if true, beta if false

	int nModelTerms(){
		int n = 0;
		for(int i=0;i<this->stats.size();i++){
			n += this->stats[i]->vStatistics().size();
		}
		return n;
	}
public:
	ReModel() : Model<Engine>(){
		//std::cout << "rm1";
		betas = ParamPtr(new std::vector<double>());
		centers = ParamPtr(new std::vector<double>());
		isThetaDep = BoolPtr(new bool);
		*isThetaDep = true;
	}

	ReModel(BinaryNet<Engine>& network) : Model<Engine>(network){
		//std::cout << "rm2";
		betas = ParamPtr(new std::vector<double>());
		centers = ParamPtr(new std::vector<double>());
		isThetaDep = BoolPtr(new bool);
		*isThetaDep = true;

		boost::shared_ptr< BinaryNet<Engine> > n(new BinaryNet<Engine>(network));
		/*this->net = n;
		this->randomGraph = boost::shared_ptr< bool >(new bool);
		this->randomDiscreteVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
		this->randomContinVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
		*this->randomGraph = true;*/
	}

	ReModel(const ReModel<Engine>& mod) : Model<Engine>(mod){
		//std::cout << "rm3";
		betas = mod.betas;
		centers = mod.centers;
		isThetaDep = mod.isThetaDep;

		/*this->stats = mod.stats;
		this->offsets = mod.offsets;
		this->net = mod.net;
		this->randomGraph = mod.randomGraph;
		this->randomDiscreteVariables = mod.randomDiscreteVariables;
		this->randomContinVariables = mod.randomContinVariables;*/
		/*if(const ReModel* m = dynamic_cast<const ReModel*>(&mod)){
			betas = m->betas;
			centers = m->centers;
			isThetaDep = m->isThetaDep;
		}*/

	}

	/*!
	 * if deep, then the model statistics are de-aliased
	 */
	ReModel(const ReModel<Engine>& mod, bool deep)  : Model<Engine>(mod, deep){
		//std::cout << "rm4";
		betas = mod.betas;
		centers = mod.centers;
		isThetaDep = mod.isThetaDep;
		if(deep){
			betas = ParamPtr(new std::vector<double>());
			centers = ParamPtr(new std::vector<double>());
			isThetaDep = BoolPtr(new bool);

			for(int i=0;i<mod.betas->size();i++)
				betas->push_back(mod.betas->at(i));
			for(int i=0;i<mod.centers->size();i++)
				centers->push_back(mod.centers->at(i));
			*isThetaDep = *mod.isThetaDep;
		}
		/*this->stats = mod.stats;
		this->offsets = mod.offsets;
		this->net = mod.net;
		this->randomGraph = mod.randomGraph;
		this->randomDiscreteVariables = mod.randomDiscreteVariables;
		this->randomContinVariables = mod.randomContinVariables;
		if(deep){
			for(int i=0;i<this->stats.size();i++)
				this->stats[i] = this->stats[i]->vClone();
			for(int i=0;i<this->offsets.size();i++)
				this->offsets[i] = this->offsets[i]->vClone();
			this->randomGraph = boost::shared_ptr< bool >(new bool);
			this->randomDiscreteVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
			this->randomContinVariables = boost::shared_ptr< std::vector<int> >(new std::vector<int>) ;
			*this->randomGraph = *mod.randomGraph;
			*this->randomDiscreteVariables = *mod.randomDiscreteVariables;
			*this->randomContinVariables = *mod.randomContinVariables;
		}*/
	}

	virtual ~ReModel(){}

	/*!
	 * R constructor for RCPP
	 *
	 */
	ReModel(SEXP sexp) : Model<Engine>(sexp){
		//std::cout << "rm5";
		boost::shared_ptr<ReModel> xp = unwrapRobject< ReModel<Engine> >(sexp);
		betas = xp->betas;
		centers = xp->centers;
		isThetaDep = xp->isThetaDep;

		/*this->stats = xp->stats;
		this->offsets = xp->offsets;
		this->net = xp->net;
		this->randomGraph = xp->randomGraph;
		this->randomDiscreteVariables = xp->randomDiscreteVariables;
		this->randomContinVariables = xp->randomContinVariables;*/
	}

	/*!
	 * coerce to R object. for RCPP
	 */
	operator SEXP() const{
		//std::cout << "rmWrap";
		return wrapInReferenceClass(*this,Engine::engineName() + "ReModel");
	}

	virtual ShallowCopyable* vShallowCopyUnsafe() const{
		return new ReModel(*this);
	}

	virtual boost::shared_ptr< Model<Engine> > vClone() const{
		return boost::shared_ptr< Model<Engine> >(new ReModel<Engine>(*this, true));
	}

	void  setBetas(std::vector<double> newBetas){
		if(nModelTerms() != newBetas.size())
			Rf_error("ReModel::setBetas : size mismatch");
		betas = ParamPtr(new std::vector<double>(newBetas));
	}

	void thetaDependent(bool td){
		isThetaDep = BoolPtr(new bool(td));
	}

	bool isThetaDependent(){
		return *isThetaDep;
	}

	std::vector<double> betaParams(){
		return *betas;
	}

	std::vector<double> centerParams(){
		return *centers;
	}

	void  setCenters(std::vector<double> newCenters){
		if(nModelTerms() != newCenters.size())
			Rf_error("ReModel::setCenters : size mismatch");
		centers = ParamPtr(new std::vector<double>(newCenters));
		//*centers = newCenters;
	}

	virtual double vLogLik(){
		double ll = 0.0;
		double s, t, par;
		int nStats = 0;
		int index = 0;
		for(int i=0;i<this->stats.size();i++){
			nStats = this->stats[i]->vStatistics().size();
			for(int j = 0; j < nStats; j++){
				s = this->stats[i]->vStatistics()[j];
				t = this->stats[i]->vTheta()[j];
				if(*isThetaDep)
					par = (t / betas->at(index)) * (t / betas->at(index));
				else
					par = betas->at(index);
				ll += s * t - par * (centers->at(index) - s) * (centers->at(index) - s);
				index++;
			}

		}
		for(int i=0;i<this->offsets.size();i++){
			ll += this->offsets[i]->vLogLik();
		}
		return ll;
	}
};

#include <Rcpp.h>

}

#endif /* MODELH_ */
