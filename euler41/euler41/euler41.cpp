#include "stdafx.h"
#include <iostream>
#include <vector>
#include <time.h>
#include "c:\headers\alexbagnall_util.h"

using namespace std;

bool isPandigital(string);


int main()
{
	time_t start = clock();
	for (int n = 7654321;; n-=2)
	{
		if (isPrime(n))
		{
			if (isPandigital(toString(n)))
			{
				cout << n << endl;
				break;
			}
		}
	}

	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

bool isPandigital(string str)
{
	vector<char> chars;
	vector<bool> found;

	for (int i = 1; i <= str.length(); i++)
	{
		chars.push_back((char)i+48);
		found.push_back(false);
	}

	for (int i = 0; i < str.length(); i++)
		for (int s = 0; s < found.size(); s++)
			if (str[i] == chars[s])
				found[s] = true;

	for (int i = 0; i < found.size(); i++)
		if (!found[i])
			return false;

	return true;
}