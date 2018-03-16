#ifndef MinDegree_H
#define MinDegree_H

#include <Rcpp.h>

#include<lolog.h>
#include<Rcpp.h>
#include<vector>
		
using namespace lolog;
using namespace Rcpp;
using namespace std;


/**
* An example lolog statistic, defined as the number of nodes
* with degree greater than or equal to "degree"
*/
template<class Engine>
class MinDegree : public Stat< Engine > {
public:
	int degree; //the minimum degree
	
	//Constructor
	MinDegree(){}
	
	//Parse parameters
	MinDegree(List params){
		try{
			degree = as< int >(params(0));
		}catch(...){
			::Rf_error("MinDegree error: please specify the degree");
		}
	}
	
	//A little boiler plate for StatController.h
	virtual Stat<Engine>* create(List params) const{ return new MinDegree(params);}
	virtual Stat<Engine>* cloneUnsafe(){ return new MinDegree<Engine>(*this);}
	
	//The name 
	virtual string name(){return "minDegree";}
	
	//Calculate the statistic
	virtual void calculate(const BinaryNet<Engine>& net){
		vector<double> v(1,0);
		this->stats=v;
		if(this->thetas.size()!=1)
			this->thetas = v;
		for(int i=0;i<net.size();i++)
			if(net.degree(i)>=degree)
				this->stats[0]++;
	}
	
	//Update the statistic given a dyad toggle
	virtual void dyadUpdate(const BinaryNet<Engine>& net, int from, int to){
		if(!net.hasEdge(from,to)){
			if(net.degree(from)==degree-1)
				this->stats[0]++;
			if(net.degree(to)==degree-1)
				this->stats[0]++;
		}else{
			if(net.degree(from)==degree)
				this->stats[0]--;
			if(net.degree(to)==degree)
				this->stats[0]--;		
		}
	}
	
	//Do nothing for vertex variable toggles
	virtual void discreteVertexUpdate(const BinaryNet<Engine>& net, int vert,
	int variable, int newValue){}		
	virtual void continVertexUpdate(const BinaryNet<Engine>& net, int vert,
	int variable, double newValue){}
};

/**
* This function registers the new MinDegree statistic so that
* it can be used in lolog formula.
*
* RcppExport means this function can be called from R using
* .C("registerMinDegree")
* see: .onLoad in zzz.R
*/
RcppExport void registerMinDegree();

#endif
