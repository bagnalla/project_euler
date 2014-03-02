#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <deque>

using namespace std;

const int NUMSIZE = 50;

template <class T>
inline std::string toString(const T&);


int main()
{
	deque<string> numbers;
	string temp, result;

	ifstream in;
	in.open("c:\\euler13.txt");
	for (int i = 0; i < 100; i++)
	{
		in >> temp;
		numbers.push_back(temp);
	}
	in.close();

	int tempNum = 0;

	for (int i = NUMSIZE-1; i >= 0; i--)
	{
		for (int s = 0; s < 100; s++)
		{
			tempNum += (int)numbers[s][i] - 48;
		}

		temp = toString(tempNum);
		result = temp.substr(temp.length()-1, 1) + result;
		tempNum = atoi(temp.substr(0, temp.length()-1).c_str());
	}

	temp = toString(tempNum);

	for (int i = temp.size()-1; i >= 0; i--)
	{
		result = temp.substr(i, 1) + result;
	}

	cout << result << endl;

	system("pause");
}



//converts any primitive to its string representation
#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}