#include "stdafx.h"
#include <iostream>
#include <set>
#include "BigIntegerLibrary.hh"
using namespace std;

BigInteger BigIntegerPow(int, int);


int main()
{
	set<BigInteger> sequence;

	for (int a = 2; a <= 100; a++)
		for (int b = 2; b <= 100; b++)
			sequence.insert(BigIntegerPow(a, b));

	cout << sequence.size() << endl;
	system("pause");
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