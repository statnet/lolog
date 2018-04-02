//#define INSIDE
#ifdef INSIDE

#include <vector>
#include <Rcpp.h>
#include "BinaryNet.h"
#include <RInside.h> // for the embedded R via RInside
#include "Stat.h"
#include "Stats.h"
#include "StatController.h"
#include "Offset.h"
#include "Offsets.h"
#include "Constraint.h"
#include "Constraints.h"
#include "Model.h"
#include "VarAttrib.h"
#include <memory>
#include <boost/shared_ptr.hpp>
#include <assert.h>
#include "tests.h"
#include <boost/container/flat_set.hpp>
#include <ctime>
#undef NDEBUG
using namespace Rcpp;
using namespace std;
using namespace lolog;


RcppExport SEXP _rcpp_module_boot_lolog(){return NULL;}
/*!
 * Main entry point when called from outside R (with R embedded via RInside).
 */
int main(int argc, char *argv[]) {
/**/
    //RInside R(argc, argv);              // create an embedded R instance
	RInside Rins(argc, argv);
    initStats();
    /*
    std::string txt = "Hello, world!\n";// assign a standard C++ string to 'txt'
    R.assign( txt, "txt");              // assign string var to R variable 'txt'
    std::string evalstr = "cat(txt)";
    for (int i=0; i<1e1; i++) {
        R.parseEvalQ(evalstr);          // eval the init string, ignoring any returns
    }
    evalstr = "txt <- \"foo\\n\"";
    for (int i=0; i<1e6; i++) {
        R.parseEvalQ(evalstr);          // eval the init string, ignoring any returns
    }
    evalstr = "cat(txt)";
    for (int i=0; i<1e1; i++) {
        R.parseEvalQ(evalstr);          // eval the init string, ignoring any returns
    }*/
    //quasiTest(R);
    tests::runLologTests();
    //flatSetTest();
    //togglerTest( R);
    //changeStatTest(R);
    //constraintTest(R);
    //togglerTest(R);
    //tetradTest(R);
    //rdsOffsetTest(R);
    //cout << expectedSqrtHypergeometric(14,0,14);
    exit(0);
    return 0;
}


#endif
