#ifdef _MATRIX__H_
#define _MATRIX__H_
#include <iostream>

template <typename T> class matrix{
	int row; // m
	int col; // n
	T **contetns;
public:

	static int mcount; // how many matrices existed
	static int mpro; // how many matrices produced

	extern matrix(int col = 0;int row = 0;T** contetns = nullptr); // ctor
	matrix(const matrix& other); // copy ctor
	matrix(matrix&& other); // move ctor
	~matrix(); // dtor
	matrix &operator=(const matrix& other); // copy assng't op
	matrix &operator=(matrix&& other); // move assng't op
	int column() const;
	int row() const;
	int ij_th(const int& i,const int& j) const;
	void swap(matrix& other);
	void ref(); // row echelon form
	void rref(); // row reduced echelon form
	void t(); // transfose
	//void inverse(); // inverse if possible
	bool operator==(const matrix& other) const;
	bool operator!=(const matrix& other) const;
	void operator+=(const matrix& other);
	void operator-=(const matrix& other);
	void operator/=(const int& k);
	void operator*=(const int& k);
	void operator*=(const matrix& other);
	void append(const matrix& other);
	matrix sol(matrix& other);

	friend matrix operator+(const matrix& m1,const matrix& m2);
	friend matrix operator-(const matrix& m1,const matrix& m1);
	friend matrix operator*(const matrix& m1,const matrix& m2);
	friend matrix operator*(const matrix& m,const int& k);
	friend matrix operator*(const int& k,const matrix& m);
	friend std::ostream &operator<<(std::ostream& out,const matrix& m);
	friend bool operator>>(std::istream& in,matrix& m);
};

#endif