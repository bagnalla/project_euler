#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

void loadChars(vector<int>&);
string checkFirstWord(vector<int>&, string);
int xor(int, int);

int main()
{
	string keyCharacters = "abcdefghijklmnopqrstuvwxyz";
	vector<int> chars;
	string key;

	loadChars(chars);

	ofstream out;
	out.open("out.txt");

	for (int a = 0; a < keyCharacters.length(); a++)
	{
		for (int b = 0; b < keyCharacters.length(); b++)
		{
			for (int c = 0; c < keyCharacters.length(); c++)
			{
				key = keyCharacters[a];
				key += keyCharacters[b];
				key += keyCharacters[c];
				out << key << " " << checkFirstWord(chars, key) << endl;
			}
		}
	}
	out.close();

	cout << (char)xor(chars[0], (int)'a') << endl;
	cout << (char)xor(chars[1], (int)'b') << endl;
	cout << (char)xor(chars[2], (int)'c') << endl;
	cout << (char)xor(chars[3], (int)'a') << endl;
	cout << (char)xor(chars[4], (int)'b') << endl;
	
	system("pause");
}

void loadChars(vector<int> &chars)
{
	ifstream in;
	in.open("c:\\cipher1.txt");
	char c;
	int temp;
	string tempString;
	do
	{
		in >> c;
		if (in.eof())
			break;
		if (c == ',')
		{
			temp = atoi(tempString.c_str());
			chars.push_back(temp);
			tempString = "";
		}
		else
			tempString += c;
	}
	while (!in.eof());
	temp = atoi(tempString.c_str());
	chars.push_back(temp);
	in.close();
}

int xor(int a, int b)
{
	return (a | b);
}

string checkFirstWord(vector<int> &chars, string key)
{
	string word;
	char c;

	for (int i = 0; i < chars.size(); i++)
	{
		//if (chars[i] == 0)
		//	break;
		c = (char)xor(chars[i], (int)key[i%key.length()]);
		word += c;
	}

	return word;
}