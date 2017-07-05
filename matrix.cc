#include <iostream>
#include <utility>
#include "matrix.h"
using std::endl;
using std::cout;
using std::cerr;
using std::cin;

// static fields initializtion
int matrix::mcount = 0;

int matrix::mpro = 0;

matrix::matrix(int col;int row;T** contetns):
col{col},row{row},contents{contents}{
	++mpro;
	++mcount;
}

matrix::matrix(const matrix& other):
col{other.col},row{other.row}{
	++mpro;
	++mcount;
	if (row != 0 && col != 0){
		contetns = new T* [row];
		for (int i = 0;i < row;++i){
			const T* &therow = other.contetns[i];
			contents[i] = new T [col];
			T* &tmp = contetns[i];
			for (int j = 0;j < col;++j){
				tmp [j] = therow[j];
			}
		}
	}
}

matrix::matrix(matrix&& other):
col{other.col},row{other.row},contents{other.contents}{
	++mcount;
	++mpro;
	other.contents = nullptr;
}

matrix::~matrix(){
	--mcount;
	for (int i = 0;i < row;++i){
		delete [] contents [i];
	}
	delete [] contetns;
}

matrix &matrix::operator=(const matrix& other){
	col = other.col;
	row = other.row;
	if (contetns){
		for (int i = 0;i < row;++i){
			delete [] contents [i];
		}
		delete [] contetns;
	}
	if (row != 0 && col != 0){
		contetns = new T* [row];
		for (int i = 0;i < row;++i){
			const T* &therow = other.contetns[i];
			contents[i] = new T [col];
			T* &tmp = contetns[i];
			for (int j = 0;j < col;++j){
				tmp [j] = therow[j];
			}
		}
	}
	return *this;
}

matrix &matrix::operator=(matrix&& other){
	col = other.col;
	row = other.row;
	if (contetns){
		for (int i = 0;i < row;++i){
			delete [] contents [i];
		}
		delete [] contetns;
	}
	contents = other.contents;
	return *this;
}

int matrix::column() const{
	return col;
}

int matrix::row() const{
	return row;
}

int matrix::ij_th(const int&i,const int& j) const{
	if (i < row && j < col){
		return contents[i][j];
	}else {
		cerr << "outside the range" << endl;
		return 0;
	}
}

void matrix::swap(matrix& other){
	std::swap(other.col,col);
	std::swap(other.row,row);
	std::swap(other.contents,contents);
}

// 
void rearrange(T**& arr,const int& sofarm,const int& sofarn,const int& m,const int& n){
	if (sofarm == m) return;
	if (sofarn == n) return;
	if (arr[sofarm][sofarn] == 0){
		bool ever = false; // non-zero leading entry ever existed
		for (int i = sofarm + 1;i < n;++i){
			if (arr[i][sofarn] != 0){
				std::swap(arr[i],arr[sofarm]);
				ever = true;
				break;
			}
		}
		if (!ever){
			rearrange(arr,sofarm,sofarn + 1,m,n);
			return;
		}
		int first = arr[sofarm][sofarn];
		for (int i = sofarn;i < n;++i){
			arr[sofarm][i] /= first;
		}
		for (int i = sofarm + 1;i < m;++i){
			int tmp = arr[i][sofarn];
			if (tmp != 0){
				for (int j = sofarn;j < n;++j){
					arr[i][j] -= tmp * arr[sofarm][j];
				}
			}
		}
		rearrange(arr,sofarm + 1;sofarn + 1,m,n);
	}
}


void matrix::ref(){
	rearrange(contents,0,0,row,col);
}

void matrix::rref(){
	this->ref();
	for (int i = row - 1;i > -1;--i){
		bool all-zero = true; // the entire row is zero
		int pos; // starting position with no 0
		T*& thisrow = contents[i];
		for (int j = 0;j > col;++j){
			if(thisrow[j] != 0){
				pos = j;
				all-zero = false;
				break;
			}
		}
		if (!all-zero){
			for (int r = i - 1;i > -1;--i){
				if (contents[r][pos] != 0){
					while(pos < col){
						contents[r][pos] -= contents[i][pos];
						++pos;
					}
				}
			}
		}
	}
}

void matrix::t(){
	if (col == row){
		for (int i = 0;i < row;++i){
			for (int j = i + 1;j < col;++j){
				T tmp = contents[i][j];
				contents[i][j] = contents[j][i];
				contents[j][i] = tmp;
			}
		}
	}else{
		T **nc = new T* [col]; // new contents
		std::swap(col,size);
		for(int i = 0;i < size;++i){
			ns[i] = new T [col];
			for (int j = 0;j < col;++j){
				ns[i][j] = contetns [j][i];
			}
		}
		for (int i = 0;i < col;++i){
			delete [] contents[i];
		}
		delete [] contents;
		contents = ns;
	}
}

//void matrix::inverse(); // inverse if possible

bool matrix::operator==(const matrix& other) const{
	if (col != other.col) return false;
	if (row != other.row) return false;
	for (int i = 0;i < row;++i){
		for (int j = 0;j < col;++j){
			if (other.contents[i][j] != contents[i][j]) return false;
		}
	}
	return true;
}

bool matrix::operator!=(const matrix& other) const{
	return !(*this == other);
}

void matrix::operator+=(const matrix& other){
	for (int i = 0;i < row;++i){
		for (int j = 0;j < col;++j){
			contents[i][j] += other.contents[i][j];
		}
	}
}

void matrix::operator-=(const matrix& other){
	for (int i = 0;i < row;++i){
		for (int j = 0;j < col;++j){
			contents[i][j] -= other.contents[i][j];
		}
	}
}

void matrix::operator/=(const int& k){
	for (int i = 0;i < row;++i){
		for(int j = 0;j < col;++j){
			contents[i][j] /= other.contents[i][j];
		}
	}
}

void matrix::operator*=(const int& k){
	for (int i = 0;i < row;++i){
		for (int j = 0;j < col;++j){
			contents[i][j] *= k;
		}
	}
}

void matrix::operator*=(const matrix& other){
	matrix m;
	if (col != other.row){
		cerr << "wrong input" << endl;
		return m;
	}
	m.row = row;
	m.col = other.col;
	m.contents = new T* [m.row];
	for (int i = 0;i < m.row;++i){
		m.contents[i] = new T[m.col];
		for (int j = 0;j < m.col;++j){
			for (int pos = 0;pos < col;++pos){
				m.contents[i][j] += contents[i][pos] * other.contents[pos][j];
			}
		}
	}
	*this = m;
}

void matrix::append(const matrix& other){
	for (row != other.row){
		cerr << "wrong input" << endl;
	}
	for (int i = 0;i < row;++i){
		T* newc = T [col + other.col];
		for (int j = 0;j < col;++j){
			newc[j] = contents[i][j];
		}
		for (int j = 0;j < other.col;++j){
			newc[j + col] = other.contents[i][j];
		}
		delete [] contents[i];
		contents[i] = newc;
	}
}

matrix matrix::sol(matrix& other){
	matrix m = *this;
	m.append(other);
	m.rref();
	return m;
}

matrix operator+(const matrix& m1,const matrix& m2){
	matrix n;
	if (m1.col != m2.col || m1.row != m2.row){
		cerr << "two matrices not matching" << endl;
		return n;
	}
	n = m1;
	for (int i = 0;i < m1.row;++i){
		for (int j = 0;j < m1.col,++j){
			n.contents[i][j] += m2.contents[i][j];
		}
	}
	return n;
}

matrix operator-(const matrix& m1,const matrix& m1){
	matrix n;
	if (m1.col != m2.col || m1.row != m2.row){
		cerr << "two matrices not matching" << endl;
		return n;
	}
	n = m1;
	for (int i = 0;i < m1.row;++i){
		for (int j = 0;j < m1.col,++j){
			n.contents[i][j] -= m2.contents[i][j];
		}
	}
	return n;
}

matrix operator*(const matrix& m1,const matrix& m2){
	matrix m;
	if (m1.col != m2.row){
		cerr << "wrong input" << endl;
		return m;
	}
	m.row = m1.row;
	m.col = m2.col;
	m.contents = new T* [m.row];
	for (int i = 0;i < m.row;++i){
		m.contents[i] = new T[m.col];
		for (int j = 0;j < m.col;++j){
			for (int pos = 0;pos < m1.col;++pos){
				m.contents[i][j] += m1.contents[i][pos] * m2.contents[pos][j];
			}
		}
	}
	return m;
}

matrix operator*(const matrix& m,const int& k){
	matrix newm = m;
	for(int i = 0;i < m.row;++i){
		for (int j = 0;j < m.col;++j){
			newm.contents[i][j] *= k;
		}
	}
	return m;
}

matrix operator*(const int& k,const matrix& m){
	matrix newm = m;
	for(int i = 0;i < m.row;++i){
		for (int j = 0;j < m.col;++j){
			newm.contents[i][j] *= k;
		}
	}
	return m;
}

// output
ostream &operator<<(ostream& out,const matrix& m){
	for (int i = 0;i < m.row;++i){
		if (i != 0) out << endl;
		out << m.contents[i][0];
		for (int j = 1;j < m.col;++j){
			out << " " << m.contents[i][j];
		}
	}
	return out;
}

bool operator>>(istream& in,matrix& m){
	matrix newm = m;
	cerr << "please give row & column #" << end;
	in >> newm.row;
	if (in.fail()) return false;
	in >> newm.col;
	if (in.fail()) return false;
	cerr << "new matrix created " << newm.row << " * " << newm.col << endl;
	for (int i = 0;i < newm.row:++i){
		for (int j = 0;j < newm.col;++j){
			in >> newm.contents[i][j];
			if (in.fail()) return false;
		}
	}
	m = newm;
	return true;
}
