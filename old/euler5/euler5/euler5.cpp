#include "stdafx.h"
#include <iostream>

using namespace std;


int main()
{
	long long int i;

	for (i = 2520;; i++)
	{
		if (!(i % 11) && !(i % 12) && !(i % 13) && !(i % 14)
			&& !(i % 15) && !(i % 16) && !(i % 17) && !(i % 18) 
			&& !(i % 19) && !(i % 20))
			break;
	}

	cout << i << endl;

	system("pause");
}

