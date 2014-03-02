#include "stdafx.h"
#include <iostream>
#include <time.h>
#include "c:\headers\alexbagnall_util.h"

using namespace std;

bool isCoolio(string);


int main()
{
	time_t start = clock();
	string temp;
	int largest = 0;

	for (int n = 2; n <= 9; n++)
	{
		for (int i = 1;; i++)
		{
			temp = "";
			for (int s = 1; s <= n; s++)
				temp += toString(i*s);

			if (temp.length() > 9)
				break;

			if (isCoolio(temp))
			{
				if (atoi(temp.c_str()) > largest)
					largest = atoi(temp.c_str());
			}
		}
	}

	cout << largest << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

bool isCoolio(string str)
{
	if (str.length() != 9)
		return false;

	string chars = "123456789";
	bool found[9] = {false};

	for (int i = 0; i < str.length(); i++)
		for (int s = 0; s < chars.length(); s++)
			if (str[i] == chars[s])
				found[s] = true;

	for (int i = 0; i < chars.length(); i++)
		if (!found[i])
			return false;

	return true;
}