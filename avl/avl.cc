#include "avl.h"

AVL::AVL():
key{0},element{0},parent{nullptr},lc{nullptr},rc{nullptr},
BI{0},height{0}{}

AVL::AVL(const int& key, const int& element):
key{key},element{element},parent{nullptr},lc{nullptr},rc{nullptr},
BI{0},height{0}{}

AVL::~AVL(){
	delete lc;
	delete rc;
}

void AVL::setBI(){
}

void AVL::setH(){
	if (parent->lc == nullptr || parent->rc == nullptr){
		parent->setBI();
	}
}

void AVL::setParent(AVL* p){
	parent = p;
}

void AVL::setLC(AVL* lc){
	this->lc = lc;
	if (lc){
		lc->setParent(this);
	}
}

void AVL::setRC(AVL* rc){
	this->rc = rc;
	if (lc){
		lc->setParent(this);
	}
}


AVL::AVL(const AVL& other):key{other.key},element{other.element}{
	BI = other.BI;
	height = other.height;
	other.parent = nullptr;
	if (other.lc){
		AVL* navl = new AVL{other.lc};
		setLC(navl);
	}
	if (other.rc){
		AVL* navl = new AVL{other.lc};
		setRC(navl);
	}
}

AVL::AVL(const AVL&& other):
key{key},element{element},parent{other.parent},lc{other.lc},rc{other.rc},
BI{other.BI},{other.},height{other.height}{
	other->setParent(nullptr);
	other->setRC(nullptr);
	other->setLC(nullptr);
	lc->setParent(this);
	rc->setParent(this);
}

AVL& AVL::operator=(const AVL& other){
	key = other.key;
	element = other.element;
	BI = other.BI;
	height = other.height;
	parent = nullptr;
	delete lc;
	delete rc;
	setLC(new AVL{other.lc});
	setRC(new AVL{other.rc});
	lc->setParent(this);
	rc->setParent(this);
	return &this;
}

AVL& AVL::operator=(const AVL&& other){
	key = other.key;
	element = other.element;
	BI = other.BI;
	height = other.height;
	parent = nullptr;
	delete lc;
	delete rc;
	setLC(ther.lc);
	setRC(other.rc);
	lc->setParent(this);
	rc->setParent(this);
	other->setParent(nullptr);
	other->setRC(nullptr);
	other->setLC(nullptr);
	return &this;
}


bool AVL::exist(const int& key){
	if (this->key = key){
		return true;
	} else if (this->key > key){
		if (lc == nullptr){
			return false;
		}
		return lc->exist(key);
	} else {
		if (rc == nullptr){
			return false;
		}
		return rc->exist(key);
	}
}


void AVL::insert(const int& key, const int& element){
	if (key == this->key){
		return; // insertion fails
	} else if (key > this->key){
		if (rc == nullptr){
			AVL navl = new AVL{key,element};
			setRC(navl);
			navl->setH();
		} else {
			rc->insert(key,element);
		}
	} else {
		if (lc == nullptr){
			AVL navl = new AVL{key,element};
			setLC(navl);
			navl->setH();
		} else {
			lc->insert(key,element);
		}
	}
}



int AVL::height(){
	return height;
}

int AVL::getKey(){
	return key;
}


int AVL::getEle(){
	return element;
}


void print();

