#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"

using namespace std;


int main()
{
	timerStart();
	int n;
	string nString;

	for (n = 1;; n++)
	{
		nString = toString(n);
		if (toString(n*6).length() > nString.length())
			continue;
		if (isPermutation(nString, toString(n*2)) && isPermutation(nString, toString(n*3))
			&& isPermutation(nString, toString(n*4)) && isPermutation(nString, toString(n*5))
			&& isPermutation(nString, toString(n*6)))
			break;
	}

	cout << n << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

