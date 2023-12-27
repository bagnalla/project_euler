#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"

using namespace std;

bool isPentagonal(unsigned long long);
bool isTriangle(unsigned long long);


int main()
{
	timerStart();
	unsigned long long n;
	for (unsigned long long i = 144;; i++)
	{
		n = i * (2 * i - 1);
		if (isPentagonal(n)) //all pentagonal are triangle
			break;
	}
	cout << n << endl;
	cout << timerStop() << endl;
	system("pause");
}

unsigned long long lastPent = 1;
bool isPentagonal(unsigned long long n)
{
	unsigned long long temp;
	for (unsigned long long i = lastPent;; i++)
	{
		temp = i * ( 3 * i - 1) / 2;
		if (temp > n)
		{
			lastPent = i;
			return false;
		}
		if (temp == n)
		{
			lastPent = i;
			return true;
		}
	}
}

/*unsigned long long lastTri = 1;
bool isTriangle(unsigned long long n)
{
	unsigned long long temp;
	for (unsigned long long i = lastTri;; i++)
	{
		temp = i * (i + 1) / 2;
		if (temp > n)
		{
			lastTri = i;
			return false;
		}
		if (temp == n)
		{
			lastTri = i;
			return true;
		}
	}
}*/