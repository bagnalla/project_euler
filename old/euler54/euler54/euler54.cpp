#include "stdafx.h"
#include <iostream>
#include <fstream>
#include "c:\headers\alexbagnall_util.h"
#include "constants.h"
#include "card.h"
#include "hand.h"

using namespace std;

string removeWhiteSpace(string);

int main()
{
	timerStart();
	int count = 0;
	string line;

	ifstream in;
	in.open("C:\\poker.txt");

	getline(in, line);

	while (!in.eof())
	{
		hand p1Hand(removeWhiteSpace(line.substr(0, 14)));
		hand p2Hand(removeWhiteSpace(line.substr(14, 15)));

		if (p1Hand.compareTo(p2Hand) == 1)
			count++;

		getline(in, line);
	}

	in.close();

	cout << count << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

string removeWhiteSpace(string str)
{
	string result;
	for (int i = 0; i < str.length(); i++)
		if (str[i] != ' ')
			result += str[i];
	return result;
}