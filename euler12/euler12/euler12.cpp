#include "stdafx.h"
#include <iostream>

using namespace std;


int main()
{
	int triangleNum = 0, divisors;

	for (int i = 1;; i++)
	{
		triangleNum += i;
		divisors = 0;

		for (int s = 1; s <= triangleNum/2; s++)
		{
			if (!(triangleNum % s))
			{
				divisors++;
			}
		}
		//cout << triangleNum << " has " << divisors << " divisors" << endl;
		if (++divisors > 500)
			break;
	}

	cout << triangleNum << endl;

	system("pause");
}

