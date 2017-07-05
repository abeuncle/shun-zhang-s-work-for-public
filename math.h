#ifndef __MATH_H__
#define __MATH_H__
#include <vector>
#include <functional>

class Fraction;

std::vector<int> getVec(const int& num);

std::vector<int> getVec(const int& from, const int& to);

int gcd (const int& a, const int& b); // greatest common divisor

int factorial(const int& num);

std::vector<int> factors(const int& num);

bool prime(const int& num);

double log(const double& num, const double& base);

double ln(const double& num);

bool mod(const int& a, const int& b, const int& m);

std::vector <int> solMod(const std::function<int(int)>& f,const int& b, const int& m);

void lcon(const int& num);

Fraction exp(const Fraction& i, const int& p);

//void lcon(const Fraction& f);

#endif