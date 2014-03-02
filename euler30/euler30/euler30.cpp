#include "stdafx.h"
#include <iostream>
#include <time.h>
#include "c:\headers\alexbagnall_util.h"

bool isCoolio(int);


using namespace std;


int main()
{
	time_t start = clock();
	int sum = 0;
	for (int n = 2;n < 354294; n++)
	{
		if (isCoolio(n))
		{
			sum += n;
			cout << sum << endl;
		}
	}
	//cout << sum << endl;
	cout << (clock() - start) / (double)CLOCKS_PER_SEC << " seconds." << endl;
	system("pause");
}

bool isCoolio(int n)
{
	int sum = 0, temp;
	string nstr = toString(n);
	for (int i = 0; i < nstr.length(); i++)
	{
		stringToInteger(nstr.substr(i, 1), temp);
		sum += pow((double)temp, 5);
	}
		//sum += pow((double)atoi(nstr.substr(i, 1).c_str()), 5);
	return (sum == n);

}

