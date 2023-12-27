#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;


int main()
{
	string grid[20][20];
	int product, greatestProduct = 0;

	ifstream in;
	in.open("c:\\grid.txt");
	for (int i = 0; i < 20; i++)
	{
		for (int s = 0; s < 20; s++)
		{
			in >> grid[i][s];
		}
	}
	in.close();

	for (int i = 3; i < 17; i++)
	{
		for (int s = 3; s < 17; s++)
		{
			//north
			product = atoi(grid[i][s].c_str()) * atoi(grid[i-1][s].c_str()) * 
				atoi(grid[i-2][s].c_str()) * atoi(grid[i-3][s].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//south
			product = atoi(grid[i][s].c_str()) * atoi(grid[i+1][s].c_str()) * 
				atoi(grid[i+2][s].c_str()) * atoi(grid[i+3][s].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//east
			product = atoi(grid[i][s].c_str()) * atoi(grid[i][s+1].c_str()) * 
				atoi(grid[i][s+2].c_str()) * atoi(grid[i][s+3].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//west
			product = atoi(grid[i][s].c_str()) * atoi(grid[i][s-1].c_str()) * 
				atoi(grid[i][s-2].c_str()) * atoi(grid[i][s-3].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//northwest
			product = atoi(grid[i][s].c_str()) * atoi(grid[i-1][s-1].c_str()) * 
				atoi(grid[i-2][s-2].c_str()) * atoi(grid[i-3][s-3].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//northeast
			product = atoi(grid[i][s].c_str()) * atoi(grid[i-1][s+1].c_str()) * 
				atoi(grid[i-2][s+2].c_str()) * atoi(grid[i-3][s+3].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//southwest
			product = atoi(grid[i][s].c_str()) * atoi(grid[i+1][s-1].c_str()) * 
				atoi(grid[i+2][s-2].c_str()) * atoi(grid[i+3][s-3].c_str());
			if (product > greatestProduct)
				greatestProduct = product;

			//southeast
			product = atoi(grid[i][s].c_str()) * atoi(grid[i+1][s+1].c_str()) * 
				atoi(grid[i+2][s+2].c_str()) * atoi(grid[i+3][s+3].c_str());
			if (product > greatestProduct)
				greatestProduct = product;
		}
	}

	cout << greatestProduct << endl;

	system("pause");
}

