#include "stdafx.h"
#include <iostream>
#include <time.h>
#include "C:\headers\alexbagnall_util.h"

using namespace std;

bool isCoolio(int);
int factorial(int);


int main()
{
	time_t start = clock();
	int sum = 0;

	for (int n = 3; n < 2550000; n++)
	{
		if (isCoolio(n))
		{
			sum += n;
			cout << n << endl;
		}
	}

	cout << sum << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

bool isCoolio(int n)
{
	int sum = 0;
	string nString = toString(n);

	for (int i = 0; i < nString.length(); i++)
		sum += factorial((int)nString[i]-48);

	return (sum == n);
}

int factorial(int n)
{
	if (!n)
		return 1;
	else if (n < 3)
		return n;
	for (int i = n-1; i > 1; i--)
		n *= i;
	return n;
}