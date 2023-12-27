#include "stdafx.h"
#include <iostream>
#include <string>
#include <vector>
#include <list>
#include "gmp.h"

using namespace std;

template <class T>
inline std::string toString(const T&);


int main()
{
	vector<mpz_t> fib;
	mpz_class temp;
	//mpz_t temp;
	//mpz_init(temp);

	//mpz_set_ui(temp, 1);
	fib.push_back(temp);
	fib.push_back(temp);

	int i;
	for (i = 2;; i++)
	{
		//temp = fib[i-1] + fib[i-2];
		//temp = fib.back();
		mpz_add(temp, fib[i-1], fib[i-2]);
		//list<mpz_t>::reverse_iterator it = fib.rbegin();
		//it++;
		//temp += *it;
		fib.push_back(temp);
		if (toString(temp).length() >= 1000)
			break;
	}
	cout << fib.size() << endl;
	cout << i+1 << endl;
	system("pause");
}


#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}

