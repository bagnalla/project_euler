#include "stdafx.h"
#include <iostream>
#include <time.h>
#include "C:\headers\alexbagnall_util.h"

using namespace std;

bool isCircular(int);
string rotate(string);


int main()
{
	time_t start = clock();
	int count = 1;

	for (int n = 3; n < 1000000; n+=2)
	{
		if (isCircular(n))
			count++;
	}

	cout << count << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

bool isCircular(int n)
{
	if (!isPrime(n))
		return false;

	string nString = rotate(toString(n));

	for (int i = 1; i < nString.length(); i++)
	{
		if (!isPrime(atoi(nString.c_str())))
			return false;
		nString = rotate(nString);
	}

	return true;
}

string rotate(string str)
{
	string result = str.substr(str.length()-1, 1);
	for (int i = 0; i < str.length()-1; i++)
	{
		result += str[i];
	}
	return result;
}