#include "stdafx.h"
#include <iostream>
#include "C:\headers\alexbagnall_util.h"

using namespace std;


int main()
{
	timerStart();
	int i, s;

	for (i = 1489; i < 10000; i+=2)
	{
		if (isPrime(i))
		{
			for (s = 1; s*2+i < 10000; s++)
			{
				if (isPrime(i+s) && isPermutation(toString(i), toString(i+s)) 
					&& isPrime(i+(s*2)) && isPermutation(toString(i), toString(i+(s*2))))
					goto done;
			}
		}
	}

done:

	cout << i << " " << i+s << " " << i+s+s << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}