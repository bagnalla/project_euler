#include "stdafx.h"
#include <iostream>
#include "C:/headers/alexbagnall_util.h"
#include "BigIntegerLibrary.hh"

using namespace std;

BigInteger BigIntegerFactorial(BigInteger);
BigInteger BigIntegerBinomial(BigInteger, BigInteger);
BigInteger BigIntegerPow(BigInteger, int);


int main()
{
	timerStart();
	cout << BigIntegerBinomial(40, 20) << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}


BigInteger BigIntegerFactorial(BigInteger n)
{
	if (!n.toInt())
		return 1;
	else if (n < 3)
		return n;
	for (int i = n.toInt()-1; i > 1; i--)
		n *= i;
	return n;
}

BigInteger BigIntegerBinomial(BigInteger n, BigInteger r)
{
	return BigIntegerFactorial(n) / (BigIntegerFactorial(r) * (BigIntegerFactorial(n-r)));
}

BigInteger BigIntegerPow(BigInteger num, int exponent)
{
	BigInteger result = num;
	for (int i = 1; i < exponent; i++)
		result *= num;
	return result;
}