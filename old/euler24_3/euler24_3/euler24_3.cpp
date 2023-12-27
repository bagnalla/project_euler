#include "stdafx.h"
#include <iostream>
#include <algorithm>

using namespace std;


int main()
{
	char ca[] = "0123456789";

	for (int i = 1; i < 1000000; i++)
		next_permutation(ca, ca+10);

	cout << ca << endl;

	system("pause");
}

