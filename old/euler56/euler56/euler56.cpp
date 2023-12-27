#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"
#include "BigIntegerLibrary.hh"

using namespace std;

BigInteger BigIntegerPow(int, int);
int digitalSum(string);


int main()
{
	timerStart();
	int biggestSum = 0, sum;

	for (int a = 90; a < 100; a++)
	{
		for (int b = 90; b < 100; b++)
		{
			sum = digitalSum(toString(BigIntegerPow(a, b)));
			if (sum > biggestSum)
				biggestSum = sum;
		}
	}

	cout << biggestSum << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

BigInteger BigIntegerPow(int num, int exponent)
{
	BigInteger result = num;
	for (int i = 1; i < exponent; i++)
		result *= num;
	return result;
}

int digitalSum(string s)
{
	int sum = 0;
	for (int i = 0; i < s.length(); i++)
		sum += (int)s[i]-48;
	return sum;
}