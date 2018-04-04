/*
 * ParamParser.h
 *
 *  Created on: Apr 4, 2018
 *      Author: ianfellows
 */

#ifndef PARAMPARSER_H_
#define PARAMPARSER_H_
#include <string>
#include <Rcpp.h>

namespace lolog {


class ParamParser {
	std::string name;
	Rcpp::List params;
	int nUnnamedParsed;
	int totalParsed;
protected:
	template<class T>
	T parseNext(std::string paramName, T defaultValue, bool allowDefault){
		T ret(defaultValue);

		int s = params.size();
		if(s <= nUnnamedParsed){
			::Rf_error(("Error in " + name + ": To few parameters.").c_str());
			return ret;
		}
		std::string pName;
		CharacterVector names = params.names();
		pName = names.at(nUnnamedParsed);
		if(pName == ""){
			try{
				ret = Rcpp::as<T>(params.at(nUnnamedParsed));
				totalParsed++;
			}catch(...){
				::Rf_error(("Error in " + name + ": Invalid type for parameter " + paramName).c_str());
			}
			nUnnamedParsed++;
		}else{
			bool found = false;
			for(int i=nUnnamedParsed; i < s; i++){
				pName = names.at(i);
				found = pName == paramName;
				if(found){
					try{
						ret = Rcpp::as<T>(params.at(i));
						totalParsed++;
					}catch(...){
						::Rf_error(("Error in " + name + ": Invalid type for parameter " + paramName).c_str());
					}
					continue;
				}
			}
			if(!found && !allowDefault){
				::Rf_error(("Error in " + name + ":  Required parameter " + paramName + " missing").c_str());
			}
		}
		return ret;
	}

public:
	ParamParser() : name("ParamParser"), totalParsed(0), nUnnamedParsed(0), params(){};

	ParamParser(std::string funcName) : name(funcName), totalParsed(0), nUnnamedParsed(0), params(){};

	ParamParser(std::string funcName, Rcpp::List funcParams) : name(funcName), totalParsed(0), nUnnamedParsed(0), params(funcParams){};

	virtual ~ParamParser(){};

	template<class T>
	T parseNext(std::string paramName, T defaultValue){
		return parseNext(paramName, defaultValue, true);
	}

	template<class T>
	T parseNext(std::string paramName){
		return parseNext(paramName, T(), false);
	}

	void end(){
		if(totalParsed != params.size()){
			Rf_error(("Either unknown or duplicate parameters passed to " + name).c_str());
		}
	}
};

}

#endif /* PARAMPARSER_H_ */

