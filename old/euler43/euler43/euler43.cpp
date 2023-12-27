#include "stdafx.h"
#include <iostream>
#include <algorithm>
#include "c:\headers\alexbagnall_util.h"

using namespace std;

bool isCool(char[]);


int main()
{
	unsigned long long sum = 0, temp;
	char d[] = "0123456789";

	do
	{
		if (isCool(d))
		{
			string tempString(d);
			stringToUnsignedLong(tempString, temp);
			sum += temp;
			cout << temp << endl;
		}
	} 
	while (next_permutation(d, d+10));

	cout << sum << endl;
	system("pause");
}

bool isCool(char d[])
{
	string s(d);
	if (!(s[3] % 2) && !(atoi(s.substr(2, 3).c_str()) % 3) &&!(atoi(s.substr(3, 3).c_str()) % 5)
		&& !(atoi(s.substr(4, 3).c_str()) % 7) && !(atoi(s.substr(5, 3).c_str()) % 11) &&
		!(atoi(s.substr(6, 3).c_str()) % 13) && !(atoi(s.substr(7, 3).c_str()) % 17))
		return true;
	return false;
}