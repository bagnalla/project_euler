#include "stdafx.h"
#include <iostream>

using namespace std;

int d(int);


int main()
{
	int other, sum = 0;
	for (int i = 1; i < 10000; i++)
	{
		other = d(i);
		if (d(other) == i && other != i)
			sum += i;
	}

	cout << sum << endl;

	system("pause");
}


int d(int num)
{
	int sum = 0;
	for (int i = 1; i <= num/2; i++)
	{
		if (!(num % i))
			sum += i;
	}
	return sum;
}