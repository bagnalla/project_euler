#include "stdafx.h"
#include <iostream>
#include <string>

using namespace std;


int main()
{
	string dayNames[] = {"monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"};
	int days = 0, daysTemp, sundays = 0;

	for (int i = 1901; i < 2000; i++)
	{
		if ((i % 4 == 0 && i % 100) || (i % 100 == 0 && i % 400 == 0))
			daysTemp = 366;
		else
			daysTemp = 365;

		for (int s = 0; s < daysTemp; s++)
		{
			//cout << i << " " << s << " " << dayNames[days%7] << endl;
			if (daysTemp == 365)
			{
				if (s == 0 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 31 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 59 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 90 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 120 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 151 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 181 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 212 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 243 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 273 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 304 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 334 && dayNames[days%7] == "sunday")
					sundays++;
			}
			else
			{
				if (s == 0 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 31 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 60 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 91 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 121 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 152 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 182 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 213 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 244 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 274 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 305 && dayNames[days%7] == "sunday")
					sundays++;
				else if (s == 335 && dayNames[days%7] == "sunday")
					sundays++;
			}
			days++;
		}
	}

	cout << sundays << endl;

	system("pause");
}

