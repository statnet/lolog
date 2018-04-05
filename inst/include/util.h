#ifndef UTILH_
#define UTILH_

#include <Rcpp.h>
#include <RcppCommon.h>
#include <cmath>
#include <memory>
#include<vector>
#include <boost/shared_ptr.hpp>
namespace lolog{

using namespace Rcpp;

const double doubleTolerance = .0000000001;

/*!
 * determine if two numbers are within a tolerance limit of one another
 */
inline bool near(double a, double b){
    return a <= b + doubleTolerance && a >= b - doubleTolerance;
}


/*!
 * Unwraps either an XPtr or Reference Class representation (through an Rcpp Module)
 * of a C++ class
 */
template<class T>
boost::shared_ptr<T> unwrapRobject(const SEXP& s){

    if ( TYPEOF(s) == EXTPTRSXP ) {
        XPtr<T > xp(s);
        return xp->template vShallowCopy<T>();
        //return boost::shared_ptr<T>(new T(*xp));
    }else if ( TYPEOF(s) == S4SXP ) {
        Rcpp::S4 s4obj( s );
        Rcpp::Environment env( s4obj );
        Rcpp::XPtr<T> xp( env.get(".pointer") );
        return xp->template vShallowCopy<T>();
        //return boost::shared_ptr<T>(new T(*xp));
    }
    Rcpp:Rcout << TYPEOF(s);
    ::Rf_error( "unwrapRobject: supplied object is not of correct type." );
}


/*!
 * Creates an R Reference class object from an object.
 * Object must be of a module exported class
 */
template<class T>
SEXP wrapInReferenceClass(const T& obj,std::string className){
    XPtr< T > xp = (&obj)->template vShallowCopyXPtr<T>();
    //XPtr< T > xp(new T(obj));
#ifndef INSIDE
    Language call( "new", Symbol( className ),xp);
    return call.eval();
#endif
#ifdef INSIDE
    // when called from RInside, our reference classes are not available.
    return xp;
#endif
}

//Look up tables
extern const double integerSquareRoots[1000];
extern const double expectedAnscombeValues[101][1001];

/**
 * the expectation of sqrt(x+3/8) where x is binomial
 */
inline double expectedAnscombe(double mean, int n, int k){
    if(n>=0 && n<=100 && mean>=0.0 && mean<=n){
        int index;
        if(n==0)
            index = 0;
        else
            index = round( 1000.0 * mean/((double)n) );
        return expectedAnscombeValues[n][index];//[n][index];
    }
    return sqrt(mean+3.0/8.0);
}

//n = total possible
//k = total possible successes
//draws = number of draws
inline double expectedAnscombe2(int n, int k,int draws){
    if(k==0 || draws==0){
        return 0.612372435695794;
    }

    //hypergeometric approximation
    double n1 = n;
    double k1 = k;
    if(k==1){
        double q = 1.0;
        for(int i=0;i<draws;i++){
            q *= 1.0 - k1 / (n1 - i);
        }
        return 0.612372435695794*q + 1.17260393995586*(1-q);
    }

    if(k<0){
        double s = 0.0;
        for(int i=0;i<=k;i++){
            s += sqrt(i + 0.375) * Rf_dhyper((double)i,k1,n1-k1,draws,0);
        }
        return s;
    }
    double mean = draws* k1 / n1;

    //poisson-anscombe approximation
    return sqrt(mean + 0.375);
}

/*!
 * calculates the sqrt of an integer using look-up tables
 */
inline double fastSqrt(int& integer){

    if(integer<0 || integer>=1000)
        return sqrt(integer);
    return integerSquareRoots[integer];
}


/*!
 * calculated the expectation of the square root of a hypergeometric
 * variate using a binomial approximation
 * \param m number of successes in population
 * \param n number of failures in population
 * \param k sample size
 */
inline double expectedSqrtHypergeometric(double m, double n, double k){
    if(k<.5)
        return 0;
    if(k<1.5)
        return	m/(m+n);
    const double p = m/(m+n);
    if(near(p,1.0))
        return sqrt(k);
    const double q = 1.0-p;
    const double qinv = 1.0/q;
    //binomial approximation. implemented using recursive identity.
    double res = 0.0;
    double coef = 1.0;
    double ppow = 1.0;
    double qpow = pow(q,k);
    for(int i=1;i<k+.5;i++){
        coef *= (k-i+1.0)/(double)i;
        ppow *= p;
        qpow *= qinv;
        res += coef*ppow*qpow*fastSqrt(i);
    }
    return res;
}

inline double expectedSqrtHypergeometric2(double m, double n, double k){
    if(k<.5)
        return 0;

    if(k<1.5)
        return	m/(m+n);


    const double lp = log(m/(m+n));
    const double lq = log(1.0-m/(m+n));
    //const double ratio = p/q;
    //const double qinv = 1.0/q;
    //binomial approximation. implemented using recursive identity.
    double res = 0.0;
    //double prob = pow(q,k);
    double lcoef = 0.0;
    double lppow = 0.0;
    double lqpow = k*lq;
    for(int i=1;i<k+.5;i++){
        //prob *= ratio*(k-i+1.0)/(double)i;
        //res += prob*fastSqrt(i);
        lcoef += log(k-i+1.0)-log(i);
        lppow += lp;
        lqpow -= lq;
        res += exp(lcoef+lppow+lqpow)*fastSqrt(i);
    }
    return res;
}

/*!
 * n choose k optimized for cases where n<k a lot
 */
inline double nchoosek(double n,double k){
    return (n<k) ? 0.0 : Rf_choose(n,k);
}

/*!
 * returns the first index where element occurs in vec
 */
template<class T>
inline int indexOf(T &element,std::vector<T> vec){
    for(int i=0;i<vec.size();i++){
        if(vec[i]==element)
            return i;
    }
    return -1;
}

/*!
 * Coerse a type into a string
 */
template<class T>
std::string asString(T item){
    return static_cast<std::ostringstream*>( &(std::ostringstream() << item) )->str();
}


/*!
 * hash for a pair<int,int>
 */
struct PairHash {
    inline std::size_t operator()(const std::pair<int,int> & v) const {
        return v.first + v.second * 31;
    }
};

/*!
 * Generates a sorted sample of integers from 0 - populationSize-1
 * of size sampleSize.
 *
 */
inline void sampleWithoutReplacement(
        int populationSize,    // size of set sampling from
        int sampleSize,        // size of each sample
        std::vector<int> & samples  // output, zero-offset indicies to selected items
){
    samples.resize(sampleSize);
    // Use Knuth's variable names
    int& n = sampleSize;
    int& N = populationSize;

    int t = 0; // total input records dealt with
    int m = 0; // number of items selected so far
    double u;

    while (m < n){
        u = Rf_runif(0.0,1.0); // call a uniform(0,1) random number generator

        if ( (N - t)*u >= n - m ){
            t++;
        }
        else{
            samples[m] = t;
            t++; m++;
        }
    }
}




/*!
 * An enumeriation of the types of edges
 */
enum EdgeDirection {UNDIRECTED, IN, OUT};

}
#endif /* UTILH_ */
