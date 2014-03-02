#include "stdafx.h"
#include <iostream>
#include <fstream>
#include "c:\headers\alexbagnall_util.h"

using namespace std;

int wordValue(string);
bool isTriangle(int);


int main()
{
	timerStart();
	int count = 0;
	ifstream in;
	in.open("c:\\words.txt");

	string temp;
	char c;
	while (true)
	{
		in >> c;
		if (in.eof())
			break;
		if (c == ',')
		{
			if (isTriangle(wordValue(temp)))
				count++;
			//names.push_back(temp);
			temp = "";
		}
		else if (c != '"')
			temp += c;
	}
	if (temp.length())
		if (isTriangle(wordValue(temp)))
				count++;
		//names.push_back(temp);

	in.close();

	cout << count << endl;
	cout << timerStop() << endl;
	system("pause");
}

int wordValue(string word)
{
	int value = 0;
	word = toLowerCase(word);

	for (int i = 0; i < word.length(); i++)
		value += (int)(word[i] - 96);

	return value;
}

bool isTriangle(int n)
{
	for (int i = 1, s = 1; s <= n; i++, s += i)
		if (s == n)
			return true;
	return false;
}

