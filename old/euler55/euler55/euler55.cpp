#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"
//#include "BigIntegerLibrary.hh"

using namespace std;

bool isLynchrel(int);
string reverseAndAdd(string);


int main()
{
	timerStart();
	int count = 0;

	for (int n = 1; n < 10000; n++)
	{
		if (isLynchrel(n))
			count++;
	}

	cout << count << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

bool isLynchrel(int n)
{
	string nString = toString(n);

	for (int i = 1; i < 50; i++)
	{
		nString = reverseAndAdd(nString);
		if (isPalindrome(nString))
			return false;
	}
	return true;
}

string reverseAndAdd(string n)
{
	string r = reverseString(n);
	return addStrings(n, r);
}