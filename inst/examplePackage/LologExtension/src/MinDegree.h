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
class MinDegree : public BaseStat< Engine > {
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
	
	//The name 
	virtual string name(){return "minDegree";}
	
	std::vector<std::string> statNames(){
	  std::vector<std::string> statnames(1,"minDegree");
	  return statnames;
	}
	
	//Calculate the statistic
	virtual void calculate(const BinaryNet<Engine>& net){
		vector<double> v(1,0);
		this->stats=v;
		this->lastStats = std::vector<double>(1,0.0);
		if(this->thetas.size()!=1)
			this->thetas = v;
		for(int i=0;i<net.size();i++)
			if(net.degree(i)>=degree)
				this->stats[0]++;
	}
	
	//Update the statistic given a dyad toggle
	virtual void dyadUpdate(const BinaryNet<Engine>& net,const int &from,const int &to,const std::vector<int> &order,const int &actorIndex){
	  BaseOffset<Engine>::resetLastStats();
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
	
	//Declare that this statistic is order independent
	bool isOrderIndependent(){
	  return true;
	}
	
	//Declare that this statistic is dyad independent
	bool isDyadIndependent(){
	  return false;
	}
	
};

typedef Stat<Undirected, MinDegree<Undirected> > UndirectedMinDegree;

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
