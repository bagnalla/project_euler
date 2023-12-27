#include "stdafx.h"
#include <iostream>
#include "gmp.h"
#include "c:/headers/alexbagnall_util.h"

using namespace std;


int main()
{
	timerStart();
	int count = 0;
	unsigned long long numerator = 1393, denominator = 985;

	mp_exp_t *exponent = new mp_exp_t;
	
	mpf_t n, d, multiplier, round;
	mpf_init(n);
	mpf_init(d);
	mpf_init(multiplier);
	mpf_init(round);

	mpf_set_ui(n, 1393);
	mpf_set_ui(d, 985);
	mpf_set_d(multiplier, 2.4141);
	mpf_set_d(round, .5);

	for (int i = 0; i < 992; i++)
	{
		mpf_mul(n, n, multiplier);
		mpf_mul(d, d, multiplier);

		mpf_add(n, n, round);
		mpf_add(d, d, round);

		mpf_floor(n, n);
		mpf_floor(d, d);

		string nString(mpf_get_str(NULL, exponent, 10, 0, n));
		string dString(mpf_get_str(NULL, exponent, 10, 0, d));

		if (nString.length() > dString.length())
		{
			cout << nString << "/" << dString << endl;
			count++;
		}
	}

	cout << count << endl;
	cout << timerStop()<< " seconds." << endl;


	mpf_clear(n);
	mpf_clear(d);
	mpf_clear(multiplier);


	/*for (int i = 0; i < 50; i++)
	{
		numerator = (unsigned long long)(numerator * 2.4141 + .5);
		denominator = (unsigned long long)(denominator * 2.4141 + .5);
		cout << toString(numerator) << " / " << toString(denominator) << endl;
		if (toString(numerator).length() > toString(denominator).length())
			count++;
	}*/

	//cout << count << endl;
	system("pause");
}

