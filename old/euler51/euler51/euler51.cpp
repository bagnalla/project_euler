#include "stdafx.h"
#include <iostream>
#include "C:\headers\alexbagnall_util.h"

using namespace std;

bool isCoolio(int);
int doCount(string, int);


int main()
{
	int n;

	for (n = 101;; n+=2)
		if (isPrime(n) && isCoolio(n))
			break;

	cout << n << endl;
	system("pause");
}

bool isCoolio(int n)
{
	string nString = toString(n), temp;
	int count;

	for (int i = 1; i < nString.length(); i++)
	{
		for (int s = 0; s < nString.length()-i; s++)
		{
			temp = nString;
			count = 0;
			for (int x = 0; x <= 9; x++)
			{
				temp.replace(s, i, i, (char)x+48);
				if (isPrime(atoi(temp.c_str())))
					count++;
				//cout << temp << endl;
			}
			if (count == 7)
				return true;
		}
	}
	return false;
	//if (doCount(nString, 2) == 7)
	//	return true;
	//else return false;
}

int doCount(string nString, int digits)
{
	int count, highCount = 0;
	string temp;

	for (int i = 0; i < nString.length()-1; i++)
	{
		for (int s = i+1; s < nString.length()-1; s++)
		{
			for (int p = s+1; p < nString.length()-1; p++)
			{
				//for (int u = p+1; u < nString.length()-1; u++)
				//{
					count = 0;
					temp = nString;
					for (int x = 0; x <= 9; x++)
					{
						temp.replace(i, 1, 1, (char)x+48);
						temp.replace(s, 1, 1, (char)x+48);
						temp.replace(p, 1, 1, (char)x+48);
						//temp.replace(u, 1, 1, (char)x+48);
						if (isPrime(atoi(temp.c_str())))
						{
							count++;
							//cout << temp << endl;
						}
					}
					if (count > highCount)
						highCount = count;
				//}
			}
		}
	}
	return highCount;
}