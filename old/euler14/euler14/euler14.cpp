#include "stdafx.h"
#include <iostream>
#include <vector>

using namespace std;


int main()
{
	int count, longest, longestCount = 0;
	long long int n;
	vector<int> memo;

	count = n = 1;

	for (int i = 1; i < 1000000; i++)
	{
		n = i;

		//cout << i << endl;

		for (count = 1; n != 1; count++)
		{
			if (!(n % 2))
			{
				n = n / 2;
				if (n < i)
				{
					count += memo[n-1];
					break;
				}
			}
			else
				n = n * 3 + 1;
		}

		if (count > longestCount)
		{
			longest = i;
			longestCount = count;
		}

		memo.push_back(count);
	}

	cout << longest << endl;

	system("pause");
}

