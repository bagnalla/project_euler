#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <vector>
#include "gmp.h"
#include "c:/headers/alexbagnall_util.h"



int main()
{
	timerStart();
	vector<string> lines;
	int commaIndex, exponent, largestLine;
	string line, baseString, exponentString;
	mpz_t base, result, largest;
	mpz_init(base);
	mpz_init(result);
	mpz_init_set_ui(largest, 0);

	ifstream in;
	in.open("c:\\base_exp.txt");
	do
	{
		getline(in, line);
		lines.push_back(line);
	}
	while (!in.eof());
	in.close();

	for (int i = 0; i < lines.size(); i++)
	{
		for (int s = 0; s < lines[i].length(); s++)
			if (lines[i][s] == ',')
				commaIndex = s;

		baseString = lines[i].substr(0, commaIndex);
		exponentString = lines[i].substr(commaIndex+1, lines[i].length()-commaIndex-1);

		mpz_set_str(base, baseString.c_str(), 10);
		exponent = atoi(exponentString.c_str());
		mpz_pow_ui(result, base, exponent);

		int temp = mpz_sizeinbase(result, 10) - mpz_sizeinbase(largest, 10);
		if (temp > 0)
		{
			mpz_set(largest, result);
			largestLine = i+1;
		}
		else if (temp == 0 && mpz_cmp(result, largest) == 1)
		{
			mpz_set(largest, result);
			largestLine = i+1;
		}

	}

	//cout << string(mpz_get_str(NULL, 10, largest)) << endl;
	cout << largestLine << endl;
	cout << timerStop() << " seconds." << endl;

	mpz_clear(base);
	mpz_clear(result);
	mpz_clear(largest);
	system("pause");
}

