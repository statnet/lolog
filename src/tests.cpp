#include "tests.h"
#include <Rcpp/iostream/Rstreambuf.h>
#include "test_BinaryNet.h"
#include "test_Constraints.h"
#include "test_LatentOrderLikelihood.h"
#include "test_Stats.h"

namespace lolog{
namespace tests{

std::string testContext;


void runLologTests(){
#ifdef INSIDE
	Rcpp::Rcout << "\n\t";
#endif
	testBinaryNet();
	testStats();
	testConstraints();
	testLatent();
#ifdef INSIDE
	Rcpp::Rcout << "All C++ Tests Complete\n";
#endif
}


}
}
