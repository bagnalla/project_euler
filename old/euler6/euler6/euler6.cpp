#include "stdafx.h"
#include <iostream>
#include <math.h>

using namespace std;


int main()
{
	int sumOfSquare = 0, squareOfSum = 0;

	for (int i = 1; i <= 100; i++)
	{
		sumOfSquare += pow((float)i, 2);
		squareOfSum += i;
	}

	squareOfSum = pow((float)squareOfSum, 2);

	cout << (squareOfSum - sumOfSquare) << endl;

	system("pause");
}

