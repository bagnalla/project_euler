#include "stdafx.h"
#include <iostream>
//#include <iomanip>
#include <string>
#include <math.h>
#include "BigIntegerLibrary.hh"

using namespace std;

bool isPrime(int);
template <class T>
inline std::string toString(const T&);
BigInteger BigIntegerPow(int, int);
string recurringPattern(string);


int main()
{
	string biggest;
	string temp;
	int biggestPos;

	for (int i = 2; i < 1000; i++)
	{
		if (isPrime(i))
		{
			temp = toString((BigIntegerPow(10, i)-1) / i);
			//temp = (unsigned int)(pow((double)10, i)-1) / i;
			if (recurringPattern(temp).length() > recurringPattern(biggest).length())
			{
				cout << i << endl;
				biggest = temp;
				biggestPos = i;
			}
			//cout  << (pow((double)10, i)-1) / i << endl;;
		}
	}

	cout << biggestPos << endl;
	system("pause");
}


bool isPrime(int num)
{
	for (int i = 2; i <= sqrt((double)num); i++)
	{
		if (!(num % i))
			return false;
	}
	return true;
}

#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}

BigInteger BigIntegerPow(int num, int exponent)
{
	BigInteger result = num;
	for (int i = 1; i < exponent; i++)
	{
		result *= num;
	}
	return result;
}

string recurringPattern(string str)
{
	string temp, result;
	for (int i = 0; i < str.length()/2; i++)
	{
		temp = str.substr(0, i+1);
		for (int s = temp.length(); s <= str.length()-temp.length(); s += temp.length())
		{
			if (str.substr(s, temp.length()) != temp)
				goto next;
		}
		result = temp;
		break;
		next:;
	}
	return result;
}