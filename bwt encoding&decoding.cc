#include <iostream>
#include <string>
#include <vector>
#include <map>
using namespace std;


void quickSort(vector<string>& arr, int left, int right) {
      int i = left, j = right;
      string tmp;
      string pivot = arr[(left + right) / 2];
 
      /* partition */
      while (i <= j) {
            while (arr[i] < pivot)
                  i++;
            while (arr[j] > pivot)
                  j--;
            if (i <= j) {
                  tmp = arr[i];
                  arr[i] = arr[j];
                  arr[j] = tmp;
                  i++;
                  j--;
            }
      };
 
      /* recursion */
      if (left < j)
            quickSort(arr, left, j);
      if (i < right)
            quickSort(arr, i, right);
}

void sort(vector<string>& vs){
	quickSort(vs,0,vs.size() - 1);
}


string generateBWT(const string& s){
	string ns = s + '\n';
	int y = s.length();
	vector<string> vs;
	for (int i = y;i > -1;--i){
		//cout << ns << endl;
		vs.emplace_back(ns);
		ns = ns[y] + ns.substr(0,y);
	}
	sort(vs);
	ns = "";
	for (auto& it : vs){
		ns += it[y];
	}
	return ns;
}

struct pos{
	char c;
	int index;
	pos(const char& c,const int& i):c{c},index{i}{}
	pos(const pos& p):c{p.c},index{p.index}{}
	pos& operator=(const pos& p){
		c = p.c;
		index = p.index;
		return *this;
	}
	pos():c{'\n'},index{-1}{}
	bool operator<(const pos& p){
		return (c < p.c);
	}
	bool operator>(const pos& p){
		return (c > p.c);
	}
};

ostream& operator<<(ostream& out, const pos& p){
	out << p.index << " : " << p.c << '\n';
	return out;
}


void quickSort(vector<pos>& arr, int left, int right) {
      int i = left, j = right;
      pos tmp;
      pos pivot = arr[(left + right) / 2];
 
      /* partition */
      while (i <= j) {
            while (arr[i] < pivot)
                  i++;
            while (arr[j] > pivot)
                  j--;
            if (i <= j) {
                  tmp = arr[i];
                  arr[i] = arr[j];
                  arr[j] = tmp;
                  i++;
                  j--;
            }
      };
 
      /* recursion */
      if (left < j)
            quickSort(arr, left, j);
      if (i < right)
            quickSort(arr, i, right);
}


string generateBWTdecode(const string& s){
	vector <pos> v;
	for (int i = 0;i < s.length();++i){
		pos n {s[i],i};
		v.emplace_back(n);
	}
	quickSort(v,0,s.length()-1);
	string ns;
	int i = 0;
	for (auto &it : v){
		cout << it;
		pos p = v[v[i].index];
		ns += p.c;
		i = p.index;
	}
	return ns;
}


