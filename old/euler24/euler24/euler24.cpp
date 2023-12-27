#include "stdafx.h"
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <ctime>

using namespace std;

string shuffle(string);
int randomNumber(const int);


int main()
{
	clock_t start = clock();

	set<string> permutations;
	//srand(static_cast<unsigned int>(time(NULL)));

	const string original = "0123456789";
	//string temp;

	permutations.insert(original);
	while (permutations.size() < 3628799)
	{//3628800
		//temp = shuffle(original);
		//cout << temp << " " << permutations.size() << endl;
		permutations.insert(shuffle(original));
		
		//if (permutations.size() % 1000 == 0)
		//	cout << permutations.size() << endl;
		//if (permutations.size() > 3600000)
		//	cout << permutations.size() << endl;
	}

	cout << "bakow" << endl;

	set<string>::iterator it = permutations.begin();
	for (int i = 0; i < 999999; i++)
		it++;
	cout << *it << endl;
	//permutations.erase(it, permutations.end());


	//set<string>::reverse_iterator rit = permutations.rbegin();

	//cout << *rit << endl;

	cout << (clock() - start) / (double)CLOCKS_PER_SEC << endl;
	system("pause");
}


string shuffle(string str)
{
	for (int i = 0; i < str.length(); i++)
		swap(str[i], str[randomNumber(str.length()-1)]);

	/*for (int i = 0; i < permutations.size(); i++)
		if (str == permutations[i])
			return shuffle(str);*/

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
	//if (str.length() != original.length())
	//	return false;

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