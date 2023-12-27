#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "BigIntegerLibrary.hh"

using namespace std;

string toLowerCase(string);
int nameScore(string);


int main()
{
	ifstream in;
	in.open("c:\\names.txt");

	vector<string> names;
	string temp;
	char c;

	while (true)
	{
		in >> c;
		if (in.eof())
			break;
		if (c == ',')
		{
			names.push_back(temp);
			temp = "";
		}
		else if (c != '"')
			temp += c;
	}
	if (temp.length())
		names.push_back(temp);
	in.close();

	int smallest;
	for (int i = 0; i < names.size(); i++)
	{
		smallest = i;
		for (int s = i+1; s < names.size(); s++)
		{
			if (names[s] < names[smallest])
				smallest = s;
		}
		swap(names[i], names[smallest]);
	}

	BigInteger sum = 0;
	for (int i = 0; i < names.size(); i++)
	{
		sum += nameScore(names[i]) * (i+1);
	}

	cout << sum << endl;

	system("pause");
}




int nameScore(string name)
{
	int score = 0;
	string alphabet = "abcdefghijklmnopqrstuvwxyz";

	for (int i = 0; i < name.length(); i++)
	{
		for (int s = 0; s < alphabet.length(); s++)
		{
			if (tolower(name[i]) == alphabet[s])
			{
				score += s+1;
				break;
			}
		}
	}
	return score;
}

string toLowerCase(string str)
{
	for (unsigned int i = 0; i < str.length(); i++)
		str[i] = tolower(str[i]);

	return str;
}