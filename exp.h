#include "fraction.h"
#include <vector>


class AbsExp{
protected:
	char item;
	int power;
public:
	AbsExp(const char& i);
	AbsExp(const char& c, const int& i);
	virtual ~AbsExp() = 0;
	bool operator==(const AbsExp& e);
};

class Exp: public AbsExp{
	int coef; // coeffcient
	std::vector<AbsExp> elements;
	virtual void simplify();
public:
	Exp();
	Exp(const char& i);
	Exp(const char& c, const int& i); 
	Exp operator+(const Exp& other);
	Exp operator-(const Exp& other);
	Exp operator*(const Exp& other);
};