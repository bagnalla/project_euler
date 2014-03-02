#include "stdafx.h"
#include <iostream>
#include <math.h>

using namespace std;


int main()
{
	long long int sum = 0;

	for (int i = 2;; i++)
	{
		int temp = (int)sqrt((double)i);
		for (int s = 2; s <= temp; s++)
		{
			if (!(i % s))
				goto next;
		}

		if (i >= 2000000)
			break;
		else
			sum += i;

		next:;
	}

	cout << sum << endl;

	system("pause");
}

