/*
 * tests.cpp
 *
 *  Created on: Oct 22, 2012
 *      Author: ianfellows
 */
#include "tests.h"
#include "BinaryNetTests.h"
#include "StatTests.h"
#include "ConstraintTests.h"
#include "LatentTests.h"
#include <Rcpp/iostream/Rstreambuf.h>

namespace lolog{
namespace tests{

std::string testContext;


RcppExport void runErnmTests(){
	Rcpp::Rcout << "\n\t";
	testBinaryNet();
	testStats();
	testConstraints();
	testLatent();
	Rcpp::Rcout << "All C++ Tests Complete\n";
}


}
}
