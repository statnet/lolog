/*
 * VarAttrib.h
 *
 *  Created on: Jun 9, 2011
 *      Author: ianfellows
 */

#ifndef VARATTRIBH_
#define VARATTRIBH_

#include <vector>
#include <string>

namespace lolog{

/*!
 * Vertex variable attributed
 */
class VarAttrib {
public:
	enum VarClass {DOUBLE,INTEGER, CATEGORICAL};
	VarAttrib();
	virtual ~VarAttrib();
	virtual bool isDouble();
	virtual bool isCategorical();
	virtual bool isInteger();
	virtual std::string getName();
	virtual void setName(std::string newName);
protected:
	VarClass type;
	std::string name;
};

class DiscreteAttrib : public VarAttrib{
protected:
	std::vector<std::string> labs;
	bool hasLb,hasUb;
	int lb,ub;
public:
	DiscreteAttrib();
	virtual ~DiscreteAttrib();
	virtual void setLabels(std::vector<std::string> labs);
	virtual const std::vector<std::string>& labels() const;
	virtual bool hasLowerBound();
	virtual bool hasUpperBound();
	virtual int lowerBound();
	virtual int upperBound();
	virtual void setLowerBound(int lower);
	virtual void setUpperBound(int upper);
	virtual void removeBound(bool upper);
};

class ContinAttrib : public VarAttrib{
protected:
	bool hasLb,hasUb;
	double lb,ub;
public:
	ContinAttrib();
	virtual ~ContinAttrib();
	virtual bool hasLowerBound();
	virtual bool hasUpperBound();
	virtual double lowerBound();
	virtual double upperBound();
	virtual void setLowerBound(double upper);
	virtual void setUpperBound(double lower);
	virtual void removeBound(bool upper);
};

}

#endif /* VARATTRIBH_ */
