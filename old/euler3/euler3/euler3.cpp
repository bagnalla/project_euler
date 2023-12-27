#include "stdafx.h"
#include <iostream>
#include <math.h>
#include "C:\headers\alexbagnall_util.h"

using namespace std;


int main()
{//600851475143
	long long int num = 1094555500000, factor;

	timerStart();

	long long int i;
	for ( i = 2;; i++)
	{
		// if num is not evenly divisible by i, skip it
		if (num % i)
			continue;
			
		factor = num / i;

		/*int temp = sqrt((double)factor);
		for (int s = 2; s <= temp; s++)
		{
			if (!(factor % s))
				goto next;
		}*/
		if (!isPrime(factor))
			continue;

		break;
	}

	cout << factor << endl;
	cout << i << endl;
	cout << timerStop() * 1000 << "ms.";

	system("pause");
}

