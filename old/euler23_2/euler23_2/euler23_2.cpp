#include "stdafx.h"
#include <iostream>
#include <vector>

using namespace std;

int wat(int);


int main()
{
	vector<int> abundants;

	for (int i = 1; i <= 28123; i++)
	{
		if (wat(i) == 1)
			abundants.push_back(i);
	}

	int sum = 0;

	for (int x = 1; x <= 28123; x++)
	{
		for (int i = 0; i < abundants.size(); i++)
		{
			if ((wat(x - abundants[i]) == 1))
				goto next;
		}
		//cout << x << endl;
		sum += x;
		next:;
	}

	cout << sum << endl;

	system("pause");
}


int wat(int num)
{
	if (num <= 0)
		return -2;

	int sum = 0;
	for (int i = 1; i <= num/10; i++)
	{
		if (!(num % i))
			sum += i;
	}
	if (!(num % 2))
		sum += num / 2;
	if (!(num % 3))
		sum += num / 3;
	if (!(num % 4))
		sum += num / 4;
	if (!(num % 5))
		sum += num / 5;
	if (!(num % 6))
		sum += num / 6;
	if (!(num % 7))
		sum += num / 7;
	if (!(num % 8))
		sum += num / 8;
	if (!(num % 9))
		sum += num / 9;

	if (sum == num)
		return 0;
	else if (sum > num)
		return 1;
	else
		return -1;
}
