#include "stdafx.h"
#include <iostream>
#include <math.h>
#include <time.h>

using namespace std;


int main()
{
	time_t start = clock();
	int n, a, b, c, count, highestCount = 0, highest;

	for (n = 6; n <= 1000; n++)
	{
		count = 0;
		for (a = 1; a + (a+1) < n; a++)
		{
			for (b = a+1; a + b <= n; b++)
			{
				c = n - (a + b);
				if (pow((double)a, 2) + pow((double)b, 2) == pow((double)c, 2))
					count++;
			}
		}
		if (count > highestCount)
		{
			highestCount = count;
			highest = n;
		}
	}

	cout << highest << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}