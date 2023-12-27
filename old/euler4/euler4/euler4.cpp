#include "stdafx.h"
#include <iostream>
#include <string>

using namespace std;

bool isPalindrome(string);
template <class T>
inline std::string toString(const T&);

int main()
{
	int i, s, current, highest = 0;

	for (i = 100; i < 1000; i++)
	{
		for (s = 100; s < 1000; s++)
		{
			current = i * s;
			if (isPalindrome(toString(current)))
			{
				if (current > highest)
					highest = current;
			}
		}
	}

	cout << highest << endl;

	system("pause");
}

bool isPalindrome(string str)
{
	string tempStr = "";
	for (int i = str.length()-1; i >= 0; i--)
	{
		tempStr += str[i];
	}

	return (str == tempStr);
}

//converts any primitive to its string representation
#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}