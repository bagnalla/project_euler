#include "stdafx.h"
#include <iostream>
#include <math.h>

using namespace std;


int main()
{
	int count = 0;

	int i;
	for (i = 2;; i++)
	{
		int temp = sqrt((double)i);
		for (int s = 2; s <= temp; s++)
		{
			if (!(i % s))
				goto next;
		}

		if (++count == 10001)
			break;

		next:;
	}

	cout << i << endl;

	system("pause");
}

