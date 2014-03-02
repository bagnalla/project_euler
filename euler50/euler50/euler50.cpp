#include "stdafx.h"
#include <iostream>
#include <vector>
#include "c:\headers\alexbagnall_util.h"

using namespace std;

int doCount(int);

int highestCount = 0;


int main()
{
	timerStart();
	int highestN, count;

	for (int n = 950001; n < 1000000; n+=2)
	{
		if (isPrime(n))
		{
			count = doCount(n);
			if (count > highestCount)
			{
				highestCount = count;
				highestN = n;
			}
		}
	}

	cout << highestN << endl;
	cout << timerStop() << " seconds." << endl;
	system("pause");
}

vector<int> primes;

int doCount(int n)
{
	int count = 0, sum;


	if (!primes.size())
		primes.push_back(2);

	for (int i = primes[primes.size()-1]+1; i < n; i++)
		if (isPrime(i))
			primes.push_back(i);



	for (int i = 0; i < primes.size(); i++)
	{
		if (primes.size() - i < highestCount)
			return 0;

		count = sum = 0;
		for (int s = i; s < primes.size(); s++)
		{
			count++;
			sum += primes[s];
			if (sum > n)
				break;
			if (sum == n)
				return count;
		}
	}
	return 0;
}