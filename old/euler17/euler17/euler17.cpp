#include "stdafx.h"
#include <iostream>
#include <string>

using namespace std;

int oneDigit(int);
int twoDigits(int);
int threeDigits(int);

template <class T>
inline std::string toString(const T&);


int main()
{
	int sum = 0;

	for (int i = 1; i <= 999; i++)
	{
		sum += threeDigits(i);
		//cout << threeDigits(i) << endl;
	}

	sum += 11;

	cout << sum << endl;

	system("pause");
}


int oneDigit(int num)
{
	string strings[] = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
	
	return strings[num-1].length();
}

int twoDigits(int num)
{
	string teens[] = {"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"};
	string strings[] = {"twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"};

	if (num < 10)
		return oneDigit(num);

	if (num < 20)
		return teens[num-10].length();

	if (!(num % 10))
		return strings[num/10-2].length();

	string temp = toString(num);
	return strings[num/10-2].length() + oneDigit(atoi(temp.substr(temp.length()-1, 1).c_str()));
}


int threeDigits(int num)
{
	string hundreds[] = {"onehundred", "twohundred", "threehundred", "fourhundred", 
		"fivehundred", "sixhundred", "sevenhundred", "eighthundred", "ninehundred"};

	if (num < 100)
		return twoDigits(num);

	if (!(num % 100))
		return hundreds[num/100-1].length();

	string temp = toString(num);
	return hundreds[num/100-1].length() + 3 + twoDigits(atoi(temp.substr(temp.length()-2, 2).c_str()));
}







#include <sstream>
template <class T>
inline std::string toString(const T& t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}