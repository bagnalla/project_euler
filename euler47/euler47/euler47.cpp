#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"

using namespace std;

bool isCoolio(int);


int main()
{
	int n;

	for (n = 644;; n++)
	{
		if (isCoolio(n) && isCoolio(n+1) && isCoolio(n+2) && isCoolio(n+3))
			break;
	}
}

bool isCoolio(int n)
{
	return false;
}