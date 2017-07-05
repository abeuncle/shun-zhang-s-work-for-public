
class AVL{
	int key;
	int element;
	AVL* parent;
	AVL* lc; // left child
	AVL* rc; // right child
	int BI; // balace index
	int height;
public:
	AVL(const int& key, const int& element);
	AVL();
	~AVL();
	AVL(const AVL& other); // other must be root
	AVL(const AVL&& other);
	AVL& operator=(const AVL& other);
	AVL& operator=(const AVL&& other);
	bool exist(const int& key);
	void insert(const int& key, const int& element);
	void remove(const int& key);
	int height();
	int getKey();
	int getEle();
	void print();
private:
	void setH();
	void setBI();
	void rebalanceR();
	void rebalanceL();
	bool full();
	void setParent(AVL* p);
	void setLC(AVL* lc);
	void setRC(AVL* rc);
};