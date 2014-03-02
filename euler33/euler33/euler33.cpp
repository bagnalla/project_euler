#include "stdafx.h"
#include <iostream>
#include "c:\headers\alexbagnall_util.h"

using namespace std;


int main()
{
	string aString, bString;
	int numerators = 1, denominators = 1;

	for (int a = 10; a <= 99; a++)
	{
		aString = toString(a);
		if (aString[1] == '0')
			continue;

		for (int b = a+1; b <= 99; b++)
		{
			bString = toString(b);
			if (bString[1] == '0')
				continue;

			if (aString[0] == bString[0])
			{
				if ((int)(aString[1]-48) / (double)(bString[1]-48) == a / (double)b)
				{
					cout << a << " / " << b << endl;
					numerators *= a;
					denominators *= b;
				}
			}
			else if (aString[0] == bString[1])
			{
				if ((int)(aString[1]-48) / (double)(bString[0]-48) == a / (double)b)
				{
					cout << a << " / " << b << endl;
					numerators *= a;
					denominators *= b;
				}
			}
			else if (aString[1] == bString[0])
			{
				if ((int)(aString[0]-48) / (double)(bString[1]-48) == a / (double)b)
				{
					cout << a << " / " << b << endl;
					numerators *= a;
					denominators *= b;
				}
			}
			else if (aString[1] == bString[1])
			{
				if ((int)(aString[0]-48) / (double)(bString[0]-48) == a / (double)b)
				{
					cout << a << " / " << b << endl;
					numerators *= a;
					denominators *= b;
				}
			}
		}
	}

	cout << numerators << " / " << denominators << endl;
	cout << numerators / (double)denominators << endl;

	system("pause");
}

