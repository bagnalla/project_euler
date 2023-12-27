#include "stdafx.h"
#include <iostream>
#include "c:\headers\alexbagnall_util.h"

using namespace std;


int main()
{
	int a, b, n, product, mostPrimes = 0;

	for (a = -999; a <= 999; a++)
	{
		for (b = -999; b <= 999; b++)
		{
			for (n = 0;; n++)
				if (!isPrime(pow((double)n, 2) + (a * n) + b))
					break;
			if (n > mostPrimes)
			{
				mostPrimes = n;
				product = a * b;
			}
		}
	}

	cout << product << endl;
	system("pause");
}

