#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include "c:/headers/alexbagnall_util.h"

using namespace std;


int main()
{
	timerStart();
	string line;
	vector<string> lines;

	ifstream in;
	in.open("c:\\base_exp.txt");
	do
	{
		getline(in, line);
		lines.push_back(line);
	}
	while (!in.eof());
	in.close();

	int commaIndex, base, exponent, largest = 0, largestLine;
	string baseString, exponentString;

	for (int i = 0; i < lines.size(); i++)
	{
		for (int s = 0; s < lines[i].length(); s++)
			if (lines[i][s] == ',')
				commaIndex = s;

		baseString = lines[i].substr(0, commaIndex);
		exponentString = lines[i].substr(commaIndex+1, lines[i].length()-commaIndex-1);

		base = atoi(baseString.c_str());
		exponent = atoi(exponentString.c_str());

		int temp = exponent * log((double)base);
		if (temp > largest)
		{
			largest = temp;
			largestLine = i+1;
		}
	}

	cout << largestLine << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

