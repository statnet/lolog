/*
 * VarAttrib.cpp
 *
 *  Created on: Jun 9, 2011
 *      Author: ianfellows
 */

#include "VarAttrib.h"
#include <Rcpp.h>

namespace lolog{

VarAttrib::VarAttrib() {
	type = VarAttrib::DOUBLE;
	name = "";
}

VarAttrib::~VarAttrib() {
	// TODO Auto-generated destructor stub
}

bool VarAttrib::isDouble(){
	return type==VarAttrib::DOUBLE;
}
bool VarAttrib::isCategorical(){
	return type==VarAttrib::INTEGER;
}
bool VarAttrib::isInteger(){
	return type==VarAttrib::CATEGORICAL;
}
std::string VarAttrib::getName(){
	return name;
}
void VarAttrib::setName(std::string newName){
	name = newName;
}


DiscreteAttrib::DiscreteAttrib() {
	type = VarAttrib::INTEGER;
	hasUb=false;
	hasLb=false;
	ub=0;
	lb=0;
	name="";
}

DiscreteAttrib::~DiscreteAttrib() {
	// TODO Auto-generated destructor stub
}
void DiscreteAttrib::setLabels(std::vector<std::string> l){
	labs = l;
}
const std::vector<std::string>& DiscreteAttrib::labels() const{
	return labs;
}
bool DiscreteAttrib::hasLowerBound(){
	return hasLb;
}
bool DiscreteAttrib::hasUpperBound(){
	return hasUb;
}
int DiscreteAttrib::lowerBound(){
	return lb;
}
int DiscreteAttrib::upperBound(){
	return ub;
}
void DiscreteAttrib::setLowerBound(int lower){
	try{
		if(hasUb && ub<lower)
			throw std::range_error("lower bound can not be set to be larger than upper bound");
		hasLb=true;
		lb=lower;
	} catch( std::exception& _Ex__ ) {
		forward_exception_to_r( _Ex__ );
	}
}
void DiscreteAttrib::setUpperBound(int upper){
	try{
		if(hasLb && upper<lb)
			std::range_error("upper bound can not be set to be larger than lower bound");
		hasUb=true;
		ub=upper;
	} catch( std::exception& _Ex__ ) {
		forward_exception_to_r( _Ex__ );
	}
}

void DiscreteAttrib::removeBound(bool upper){
	if(upper){
		hasUb=false;
	}else{
		hasLb=false;
	}
}



ContinAttrib::ContinAttrib() {
	type = VarAttrib::DOUBLE;
	hasUb=false;
	hasLb=false;
	ub=0;
	lb=0;
	name="";
}

ContinAttrib::~ContinAttrib() {
	// TODO Auto-generated destructor stub
}

bool ContinAttrib::hasLowerBound(){
	return hasLb;
}
bool ContinAttrib::hasUpperBound(){
	return hasUb;
}
double ContinAttrib::lowerBound(){
	return lb;
}
double ContinAttrib::upperBound(){
	return ub;
}
void ContinAttrib::setLowerBound(double lower){
	try{
		if(hasUb && ub<lower)
			throw std::range_error("lower bound can not be set to be larger than upper bound");
		hasLb=true;
		lb=lower;
	} catch( std::exception& _Ex__ ) {
		forward_exception_to_r( _Ex__ );
	}
}
void ContinAttrib::setUpperBound(double upper){
	try{
		if(hasLb && upper<lb)
			std::range_error("upper bound can not be set to be larger than lower bound");
		hasUb=true;
		ub=upper;
	} catch( std::exception& _Ex__ ) {
		forward_exception_to_r( _Ex__ );
	}
}

void ContinAttrib::removeBound(bool upper){
	if(upper){
		hasUb=false;
	}else{
		hasLb=false;
	}
}


}
