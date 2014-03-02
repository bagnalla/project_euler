#include "stdafx.h"
#include <iostream>
#include "c:\headers\alexbagnall_util.h"

using namespace std;


int main()
{
	timerStart();
	string digits;
	for (int i = 0; i < 500000; i++)
		digits += toString(i);

	int result = 1;
	for (int i = 1; i <= 1000000; i *= 10)
		result *= (int)(digits[i]-48);

	cout << result << endl;

	//cout << (int)(digits[1]-48) * (int)(digits[10]-48) * (int)(digits[100]-48) * (int)(digits[1000]-48)
	//	* (int)(digits[10000]-48) * (int)(digits[100000]-48) * (int)(digits[1000000]-48) << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

