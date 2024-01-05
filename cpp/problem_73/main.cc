// Brute force using the algorithm from the Farey sequence Wikipedia
// page for enumerating the elements of Farey sequences (function
// 'f'). Also includes an alternative way of computing the answer
// (functions 'h' and 'hh') but it turned out to be slightly
// slower. It could be further optimized though by only searching for
// the first multiple of 6 in the naive way and then repeatedly adding
// 6 to obtain the rest.

#include <cmath>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

constexpr uint N = 12000;

constexpr uint gcd(uint a, uint b) {
  while (b != 0) {
    auto t = b;
    b = a % b;
    a = t;
  }
  return a;
}

// Enumerate all of the the reduced fractions between 0 and 1 with
// denominator less than or equal to N, and count those that lie
// between 1/3 and 1/2. Based on the algorithm given in the Wikipedia
// page for Farey sequences.
uint f(uint N) {
  // 0/1, 1/n
  uint a = 0;
  uint b = 1;
  uint c = 1;
  uint d = N;

  uint count = 0;

  while (c < N) {
    if (c / (double)d > 1/3.0 && c / (double)d < 1/2.0) {
      count++;
    }
    
    uint p = floor((N + b) / d) * c - a;
    uint q = floor((N + b) / d) * d - b;

    a = c;
    b = d;
    c = p;
    d = q;
  }

  return count;
}

// Compute the number of reduced fractions between 1/3 and 1/2 with
// denominator exactly equal to d. Look for integers [2*d < n < 3*d]
// that are divisible by 6 and such that n/6 is coprime with d.
uint h(uint d) {
  uint count = 0;
  for (uint i = 2*d+1; i < 3*d; i++) {
    if (i % 6 == 0 && gcd(i/6, d) == 1) {
      count++;
    }
  }
  return count;
}

// Compute the number of reduced fractions between 1/3 and 1/2 with
// denominator less than or equal to d.
uint hh(uint d) {
  uint sum = 0;
  for (uint i = 5; i <= d; i++) {
    sum += h(i);
  }
  return sum;
}

int main() {
  cout << f(N) << endl;
  // cout << hh(N) << endl;
}
