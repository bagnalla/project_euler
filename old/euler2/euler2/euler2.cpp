#include "stdafx.h"
#include <iostream>

using namespace std;


int main()
{
	long long int num1 = 1, num2 = 1, temp, evenSum = 0;

	for (int i = 1;;i++)
	{
		temp = num2;
		num2 += num1;
		num1 = temp;

		if (!(num2 % 2))
			evenSum += num2;

		if (num2 >= 4000000)
			break;
	}

	cout << evenSum << endl;

	system("pause");
}

