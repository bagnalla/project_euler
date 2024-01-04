#include <cmath>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

constexpr uint N = 1000000 + 1;

constexpr bool isPrime(uint n) {
  if (n % 2 == 0) {
    return n == 2;
  }
  for (uint i = 3; i*i <= n; i += 2) {
    if (n % i == 0) {
      return false;
    }
  }
  return n > 1;
}

// constexpr uint gcd(uint a, uint b) {
//   while (b != 0) {
//     auto t = b;
//     b = a % b;
//     a = t;
//   }
//   return a;
// }

// uint phi(uint n) {
//   uint count = 0;
//   for (uint i = 1; i < n; i++) {
//     if (gcd(n, i) == 1) {
//       count++;
//     }
//   }
//   return count;
// }

uint f(uint N) {
  // 0/1, 1/n
  uint a = 0;
  uint b = 1;
  uint c = 1;
  uint d = N;

  uint count = 0;

  while (c < N) {
    uint p = floor((N + b) / d) * c - a;
    uint q = floor((N + b) / d) * d - b;

    // cout << a << "/" << b << " ";

    count++;
    
    a = c;
    b = d;
    c = p;
    d = q;
  }

  // cout << endl;

  return count-1;
}

static vector<uint> primes;

void init_primes() {
  primes.push_back(2);
  for (uint i = 3; i <= N; i += 2) {
    if (isPrime(i)) {
      primes.push_back(i);
    }
  }
}

static vector<set<uint>> prime_factors(N, set<uint>{});

// This could probably be optimized but it's good enough as is.
void init_prime_factors() {
  for (uint i = 0; i < primes.size(); i++) {
    for (uint j = 0; j * primes[i] < N; j++) {
      prime_factors[j*primes[i]].insert(primes[i]);
    }
  }
}

uint phi(uint n) {
  double result = n;
  for (const auto p : prime_factors[n]) {
    result *= 1 - 1.0 / p;
  }  
  return result;
}

int main() {
  cout << "Initializing primes..." << endl;
  init_primes();
  cout << "Initializing prime factors..." << endl;
  init_prime_factors();

  cout << "Computing answer..." << endl;
  unsigned long long int sum = 0;
  for (uint i = 2; i < N; i++) {
    sum += phi(i);
    // cout << i << ": " << f(i) << ", delta = " << f(i) - f(i-1) <<
    //   ", phi = " << phi(i) << ", sum = " << sum << endl;
    // cout << i << ": " << f(i) << ", sum = " << sum << endl;
  }
  cout << sum << endl;
}
