#include "stdafx.h"
#include <iostream>
#include <set>
#include <time.h>
#include "C:\headers\alexbagnall_util.h"

using namespace std;

bool isPandigital(string);


int main()
{
	time_t start = clock();
	string temp;
	set<int> products;
	int sum = 0;

	for (int a = 1;a < 100; a++)
	{
		for (int b = 1;; b++)
		{
			temp = toString(a) + toString(b) + toString(a*b);
			if (temp.length() < 9)
				continue;
			if (temp.length() > 9)
				break;
			if (isPandigital(temp))
			{
				products.insert(a*b);
				//cout << temp << endl;
			}
		}
	}

	for (set<int>::iterator it = products.begin(); it != products.end(); it++)
		sum += *it;

	cout << sum << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}


bool isPandigital(string str)
{
	char chars[] = {'1', '2', '3', '4', '5', '6', '7', '8', '9'};
	bool found[9] = {false};

	for (int i = 0; i < str.length(); i++)
		for (int s = 0; s < str.length(); s++)
			if (str[i] == chars[s])
			{
				found[s] = true;
				break;
			}

	for (int i = 0; i < 9; i++)
		if (!found[i])
			return false;

	return true;
}