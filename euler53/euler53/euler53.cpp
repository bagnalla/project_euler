#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"
#include "BigIntegerLibrary.hh"

using namespace std;

BigInteger BigIntegerFactorial(BigInteger);
BigInteger BigIntegerBinomial(BigInteger, BigInteger);


int main()
{
	timerStart();
	BigInteger c;
	int count = 0;

	for (int n = 23; n <= 100; n++)
	{
		for (int r = 1; r <= n; r++)
		{
			c = BigIntegerBinomial(n, r);
			if (c > 1000000)
				count++;
		}
	}

	cout << count << endl;
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