#include "stdafx.h"
#include <iostream>
#include <time.h>
#include "c:\headers\alexbagnall_util.h"

using namespace std;


bool isCoolio(int);


int main()
{
	time_t start = clock();
	int count = 0, sum = 0;

	for (int n = 11;; n+=2)
	{
		if (isPrime(n) && isCoolio(n))
		{
			sum += n;
			if (++count == 11)
				break;
		}
	}

	cout << sum << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

bool isCoolio(int n)
{
	string nString = toString(n);

	for (int i = 1; i < nString.length(); i++)
		if (!isPrime(atoi(nString.substr(i, nString.length()-i).c_str())))
			return false;

	for (int i = 1; i < nString.length(); i++)
		if (!isPrime(atoi(nString.substr(0, nString.length()-i).c_str())))
			return false;

	return true;
}