#include "stdafx.h"
#include <iostream>
#include <math.h>

using namespace std;


int main()
{
	int a = 1, b = 2;
	int c;
	for (a = 1; a <= 1000; a++)
	{
		for (b = a+1; b < 1000; b++)
		{
			c = 1000 - (a + b);

			if (pow((float)a, 2) + pow((float)b, 2) == pow((float)c, 2))
			cout << a << " + " << b << " = " << c << endl;
		}
	}

	system("pause");
}

