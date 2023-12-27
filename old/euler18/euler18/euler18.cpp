#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int randomNumber(const int);


int main()
{
	vector<vector<int>> triangle;
	string temp;

	ifstream in;
	in.open("C:\\euler18.txt");
	for (int i = 0; i < 15; i++)
	{
		vector<int> line;
		for (int s = 0; s <= i; s++)
		{
			in >> temp;
			line.push_back(atoi(temp.c_str()));
		}
		triangle.push_back(line);
	}
	in.close();

	unsigned long long int sum, highestSum = 0, index;

	for (int i = 0; i < 50000; i++)
	{
		sum = index = 0;
		for (int s = 0; s < triangle.size(); s++)
		{
			sum += triangle[s][index];
			index += randomNumber(1);
		}

		if (sum > highestSum)
			highestSum = sum;
	}

	cout << highestSum << endl;

	system("pause");
}


int randomNumber(const int hi)
{
	//int value=static_cast<int>(((float)rand()/RAND_MAX)*(hi+1));
	int value=(int)(((float)rand()/RAND_MAX)*(hi+1));
	if(value>hi)
		 value = 0;
	return value;
}
