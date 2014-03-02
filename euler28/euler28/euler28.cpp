#include "stdafx.h"
#include <iostream>
//#include "BigIntegerLibrary.hh"

using namespace std;


int main()
{
	int n = 1, nextRing = 0, s;
	unsigned long long sum = 1;

	for (int i = 0; i < 500; i++)
	{
		nextRing += 8;
		for (s = 1; s <= nextRing; s++)
		{
			if (nextRing * .25 == s || nextRing * .5 == s || 
				nextRing * .75 == s || nextRing == s)
				sum += (n + s);
		}
		n+= nextRing;
	}

	cout << nextRing << endl;
	cout << sum << endl;
	system("pause");
}

