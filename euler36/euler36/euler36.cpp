#include "stdafx.h"
#include <iostream>
#include <time.h>
#include "c:\headers\alexbagnall_util.h"

using namespace std;


int main()
{
	time_t start = clock();
	int sum = 0;
	string temp;

	for (int n = 1; n < 1000000; n+=2)
	{
		temp = toString(n);
		if (isPalindrome(temp) && isPalindrome(decimalToBinary(temp)))
			sum += n;
	}

	cout << sum << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

