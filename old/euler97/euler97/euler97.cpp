#include "stdafx.h"
#include <iostream>
#include "c:/headers/alexbagnall_util.h"
#include "BigIntegerLibrary.hh"

using namespace std;


int main()
{
	timerStart();

	/*BigInteger bigass = BigIntegerPow(2, 7830457);
	//* 28433 + 1;
	string s = toString(bigass);*/

	string s = stringPow(2, 783);
	//string s = "10000";
	s = multiplyStrings(s, "28433");
	s = addStrings(s, "1");

	cout << s << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}