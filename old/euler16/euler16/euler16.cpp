#include "stdafx.h"
#include <iostream>
#include <string>

using namespace std;

string multiplyStringByTwo(string);
template <class T>
inline std::string toString(const T&);


int main()
{
	string n = "2";
	for (int i = 0; i < 999; i++)
	{
		n = multiplyStringByTwo(n);
	}

	cout << n << endl << endl;

	int sum = 0;

	for (int i = 0; i < n.length(); i++)
	{
		sum += (int)n[i] - 48;
	}

	cout << sum << endl;

	system("pause");
}


string multiplyStringByTwo(string s)
{
	int tempNum = 0;
	string temp, result;

	for (int i = s.length()-1; i >= 0; i--)
	{
		tempNum += ((int)s[i] - 48) * 2;

		temp = toString(tempNum);
		result = temp.substr(temp.length()-1, 1) + result;
		tempNum = atoi(temp.substr(0, temp.length()-1).c_str());
	}

	temp = toString(tempNum);

	if (temp == "0")
		temp = "";

	for (int i = temp.size()-1; i >= 0; i--)
	{
		result = temp.substr(i, 1) + result;
	}

	return result;
}

#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}