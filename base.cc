#include <iostream>
#include <string>
//#include <cmath>
using namespace std;


//num must be positive and base be less than 10
string change_base(const int& num, const int& base){
	string r = "";
	int tmp = num;
	while (tmp != 0){
		r = to_string(tmp % base) + r;
		tmp /= base;
	}
	return r;
}

int main(){
	int i;
	while(cin >> i){
		cout << "the new base is " << i << endl;
		for (int j = 1;j < i;++j){
			char test = 'f';
			for (int ir = 1;ir <= j;++ir){
				if (test == 't'){
					cout << " ";
				}
				cout << change_base(j * ir,i);
				test = 't';
			}
			cout << endl;
		}
	}
}