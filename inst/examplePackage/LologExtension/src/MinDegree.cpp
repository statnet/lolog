// [[Rcpp::depends(lolog)]]
#include "MinDegree.h"
#include "lolog.h"


/**
* This function registers the new MinDegree statistic so that
* it can be used in lolog formula.
*
* RcppExport means this function can be called from R using
* .C("registerMinDegree")
* see: .onLoad in zzz.R
*/
RcppExport void registerMinDegree(){
	using namespace lolog;
	Rcpp::XPtr< AbstractStat<Undirected> > ps1(new UndirectedMinDegree());
	REGISTER_UNDIRECTED_STATISTIC(ps1);
}
