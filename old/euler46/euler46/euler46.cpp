#include "stdafx.h"
#include <iostream>
#include <math.h>
#include "C:\headers\alexbagnall_util.h"

using namespace std;

bool isCool(int);


int main()
{
	timerStart();
	int n;
	for (n = 9;; n+=2)
	{
		if (!isPrime(n))
		{
			if (!isCool(n))
				break;
		}
	}
	cout << n << endl;
	cout << timerStop()*1000  << "ms" << endl;
	system("pause");
}

bool isCool(int n)
{
	int temp;
	for (int i = 2; i < n; i++)
	{
		if (isPrime(i))
		{
			for (int s = 1;; s++)
			{
				temp = i + 2*pow((double)s, 2);
				if (temp > n)
					break;
				if (temp == n)
					return true;
			}
		}
	}
	return false;
}