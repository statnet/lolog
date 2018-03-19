#include "tests.h"
#include "BinaryNetTests.h"
#include "StatTests.h"
#include "ConstraintTests.h"
#include "LatentTests.h"
#include <Rcpp/iostream/Rstreambuf.h>

namespace lolog{
namespace tests{

std::string testContext;


RcppExport void runLologTests(){
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
