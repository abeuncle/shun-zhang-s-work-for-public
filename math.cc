#include <vector>
#include <cmath>
#include <iostream>
#include <functional>
#include "math.h"
#include "fraction.h"
using namespace std;

vector <int> getVec(const int& num){
	vector<int> v;
	if (num < 0){
		//cerr << "num needs to be greater than or equal to 0." << endl;
		return v;
	}
	for (int i = 1;i < num;++i){
		v.emplace_back(i);
	}
	v.emplace_back(num);
	return v;
} 

vector<int> getVec(const int& from, const int& to){
	vector<int> v;
	if (from > to){
		//cerr << "num needs to be greater than or equal to 0." << endl;
		return v;
	}
	for (int i = from;i < to;++i){
		v.emplace_back(i);
	}
	v.emplace_back(to);
	return v;
}

int factorial(const int& num){
	if (num < 0){
		//cerr << "We cannot calculate x!, when x is smaller than 0" << endl;
		return 0;
	}
	if (num == 0) return 1;
	return (num * factorial(num - 1));
}

int gcd(const int& a, const int& b){
	if(a == 0 || b == 0) return 0;
	if (a < 0 && b < 0){
		return (gcd (-1 * a, -1 * b) * -1);
	} else if (a < 0){
		return gcd(-1 * a,b);
	}else if (b < 0){
		return gcd(a,-1 * b);
	}
	if (b > a) return gcd(b,a);
	if (a % b == 0) return b;
	return gcd(b,a % b);
}

vector<int> factors(const int& num){
	vector <int> v;
	for (int i = 1;i < num;++i){
		if (num % i == 0){
			v.emplace_back(i);
		}
	}
	v.emplace_back(num);
	return v;
}

bool prime(const int& num){
	if (num < 2) return false;
	const int root = sqrt(num);
	for (int i = 2;i < root;i++){
		if (num % i == 0) return false;
	}
	return (num % root == 0);
}

double log(const double& num, const double& base){
	using std::log;
	return (std::log(num) / std::log(base));
}

double ln(const double& num){
	using std::log;
	return (std::log(num));
}

bool mod(const int& a, const int& b, const int& m){
	return ((a - b) % m == 0);
}



vector <int> solMod(const function<int(int)>& f,const int& b, const int& m){
	vector <int> v;
	for (int i = 0;i < m;++i){
		if (mod(i,b,m)) v.emplace_back(i);
	}
	return v;
}

int power(int& a, const int& b){
	int count = 0;
	while (a % b == 0){
		a /= b;
		++count;
	}
	return count;
}

void lcon(const int& num){
	int test = num;
	if (test < 0){
		cout << "-";
	}
	int i = 2;
	int p = power (test,i);
	++i;
	if (p != 0){
		cout << "(" << i << "^" << p << ")";
	}
	int root = sqrt(test) + 1;
	while (test != 1 && i < root){
		p = power (test,i);
		i += 2;
		if (p != 0){
			root = sqrt(test) + 1;
			cout << "(" << i << "^" << p << ")";
		}
	}
	if (test != 1){
		cout << "(" << test << "^1" << ")";
	}
	cout << endl;
}

Fraction exp(const Fraction& i, const int& p){
	return Fraction {static_cast<int> (pow(i.num,p)),
		static_cast<int> (pow(i.det,p))};
}

