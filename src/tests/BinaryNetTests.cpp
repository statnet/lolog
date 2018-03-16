/*
 * BinaryNetTests.cpp
 *
 *  Created on: Oct 22, 2012
 *      Author: ianfellows
 */
#include <Rcpp.h>
#ifdef INSIDE
#include "../BinaryNet.h"
#else
#include "BinaryNet.h"
#endif
#include "tests.h"

namespace lolog{
namespace tests{



template <class Engine>
void netTest(){
	Rcpp::IntegerMatrix tmp(0,2);
	BinaryNet<Engine> net(tmp,30);
	EXPECT_TRUE(net.nEdges()==0)
	net.addEdge(1,2);
	EXPECT_TRUE(net.hasEdge(1,2));
	if(!net.isDirected()){
		EXPECT_TRUE(net.hasEdge(2,1));
	}else{
		EXPECT_TRUE(!net.hasEdge(2,1));
	}
	EXPECT_TRUE(net.nEdges()==1);

	//check continuous covariates
	ContinAttrib attr;
	attr.setName("cont");
	std::vector<double> vals1(30,1.0);
	vals1[2] = 23.1;
	net.addContinVariable(vals1,attr);
	EXPECT_NEAR(net.continVariableValue(0,3),1.0);
	EXPECT_NEAR(net.continVariableValue(0,2),23.1);
	net.setContinVariableValue(0,3,51.2);
	EXPECT_NEAR(net.continVariableValue(0,3),51.2);

}

void testBinaryNet(){
	testContext = "BinaryNet";
	RUN_TEST(netTest<Directed>());
	RUN_TEST(netTest<Undirected>())

}




}
}
