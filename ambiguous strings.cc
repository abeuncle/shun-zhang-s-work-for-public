#include <iostream>
#include <vector>
#include <string>

//#include "math.h"
//#include "fraction.h"
using namespace std;

struct binary{
	vector<string> lst;
	binary(){
		lst.emplace_back("0");
		lst.emplace_back("1");
	}
	binary(const vector<string>& t):lst{t}{}
	binary(const binary& b):lst{b.lst}{}
	void rem_dup(){
		vector <string> lst2;
		const vector <string>& ll = lst;
		const int l = ll.size();
		for (int i = 0;i < l ;++i){
			bool test = true;
			for (int j = i + 1; j < l;++j){
				if (ll[i] == ll[j]) test = false;
			}
			if (test){
				lst2.emplace_back(ll[i]);
			}
		}
		lst = lst2;
	}
	int size(){return lst.size();}
};

binary uni(const binary& b1, const binary& b2){
	vector <string> tmp;
	for (auto& it : b1.lst){
		for (auto &it1 : b2.lst){
			tmp.emplace_back(it + it1);
		}
	}
	return binary {tmp};
}

bool uniq(const binary& b1){
	const vector <string>& ll = b1.lst;
	const int l = ll.size();
	for (int i = 0;i < l - 1;++i){
		for (int j = i + 1; j < l;++j){
			if (ll[i] == ll[j]) return false;
		}
	}
	return true;
}

ostream& operator<<(ostream& out,const binary& b){
	char t = 'f';
	for (auto& it : b.lst){
		if (t == 't') out << " ";
		t = 't';
		out << it;
	}
	return out;
}

binary tim(const binary& b, const int& k){
	if (k == 1) return b;
	binary tmp {b};
	for (int i = 0; i < k - 1;++i){
		tmp = uni(b,tmp);
	}
	return tmp;
}



int main(){
	vector <string> t;
	t.emplace_back("100");
	t.emplace_back("1110000");
	binary b1 {t};
	vector <string> t1;
	t1.emplace_back("110");
	t1.emplace_back("1111000");
	binary b2 {t1};
	b1 = uni(b1,b2);
	//cout << uniq (b1) << endl;
	b1 = tim(b1,3);
	cout << uniq(b1) << endl;

	for (auto & it : b1.lst){
		cout << it << " " << it.length() << endl;
	}
	//for (int i = 0;i < 4;++i){
	//	b1 = tim(b1,2);
	//	cout << b1.lst.size() << endl;
	//	if(!uniq (b1)){
	//		cout << "false" << endl;
	//		b1.rem_dup();
	//	} else {
	//		cout << "true" << endl;
	//		cout << i << endl;
	//		return 0;
	//	}
	//}
}