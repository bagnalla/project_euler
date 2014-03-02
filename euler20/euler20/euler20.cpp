#include "stdafx.h"
#include <iostream>
#include "BigIntegerLibrary.hh"

using namespace std;

template <class T>
inline std::string toString(const T&);


int main()
{
	BigInteger number = 100;

	for (int i = 99; i > 0; i--)
	{
		number *= i;
	}

	string str = toString(number);

	int sum = 0;
	for (int i = 0; i < str.length(); i++)
	{
		sum += (int)str[i] - 48;
	}

	cout << sum << endl;

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