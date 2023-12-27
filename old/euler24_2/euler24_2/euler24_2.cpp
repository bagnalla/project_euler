#include "stdafx.h"
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <ctime>

using namespace std;

string shuffle(string);
int randomNumber(const int);
bool isPermutation(string, string);
string incrementPermutation(string);
template <class T>
inline std::string toString(const T&);


int main()
{
	//srand(static_cast<unsigned int>(time(NULL)));

	vector<string> permutations;
	string original = "0123456789", temp;

	//permutations.push_back(original);
	/*for (int i = 123456789; i < 1023456789; i++)
	{
		temp = '0' + toString(i);
		if (isPermutation(temp, original))
		{
			permutations.push_back(temp);
			cout << temp << endl;
		}
	}*/

	cout << incrementPermutation(original) << endl;

	system("pause");
}


string shuffle(string str)
{
	for (int i = 0; i < str.length(); i++)
		swap(str[i], str[randomNumber(str.length()-1)]);

	//for (int i = 0; i < permutations.size(); i++)
	//	if (str == permutations[i])
	//		return shuffle(str);

	return str;
}


int randomNumber(const int hi)
{
	//int value=static_cast<int>(((float)rand()/RAND_MAX)*(hi+1));
	int value=(int)(((float)rand()/RAND_MAX)*(hi+1));
	if(value>hi)
		 value = 0;
	return value;
}

bool isPermutation(string str, string original)
{
	if (str.length() != original.length())
		return false;

	int length = original.length();

	vector<bool> found;
	for (int i = 0; i < length; i++)
		found.push_back(false);

	for (int i = 0; i < length; i++)
		for (int s = 0; s < length; s++)
			if (str[i] == original[s])
				found[s] == true;

	for (int i = 0; i < length; i++)
		if (!found[i])
			return false;

	return true;
}


string incrementPermutation(string original)
{
	string temp;
	
	for (int i = atoi(original.c_str())+1;; i++)
	{
		temp = toString(i);
		if (temp.length() < 10)
			temp = '0' + temp;
		if (isPermutation(temp, original))
		{
			//cout << "bowlop" << endl;
			return temp;
		}
		cout << "bowlop " << i << endl;
	}
	//return "poop";
}


#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}