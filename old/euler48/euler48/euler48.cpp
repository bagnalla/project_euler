#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"
#include "BigIntegerLibrary.hh"

using namespace std;

BigInteger BigIntegerPow(int, int);


int main()
{
	timerStart();
	BigInteger sum = 0;

	for (int i = 1; i <= 1000; i++)
		sum += BigIntegerPow(i, i);

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