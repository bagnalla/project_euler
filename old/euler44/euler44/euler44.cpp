#include "stdafx.h"
#include <iostream>
#include "c:\headers\alexbagnall_util.h"

using namespace std;

bool isPentagonal(int);


int main()
{
	timerStart();
	int a, b, aPent, bPent, difference;

	for (a = 1; a <= 5000; a++)
	{
		aPent = a * ( 3 * a - 1) / 2;
		for (b = a+1; b <= 5000; b++)
		{
			bPent = b * ( 3 * b - 1) / 2;
			if (isPentagonal(aPent + bPent) && isPentagonal(bPent - aPent))
				goto done;
		}
	}

done:
	cout << bPent - aPent << endl;
	cout << timerStop() << endl;
	system("pause");
}

bool isPentagonal(int n)
{
	int temp;
	for (int i = 1;; i++)
	{
		temp = i * ( 3 * i - 1) / 2;
		if (temp > n)
			return false;
		if (temp == n)
			return true;
	}
}