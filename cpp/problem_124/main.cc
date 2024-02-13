// Pretty easy using code I already wrote for problem 72 for
// pre-computing all the prime factors of numbers up to N. Runs in
// about 40ms in my Linux VM.

#include <iostream>
#include <vector>

using namespace std;

constexpr uint N = 100000;

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

static vector<uint> primes;

void init_primes() {
  primes.push_back(2);
  for (uint i = 3; i <= N+1; i += 2) {
    if (isPrime(i)) {
      primes.push_back(i);
    }
  }
}

static vector<vector<uint>> prime_factors(N+1, vector<uint>{});

// This could probably be optimized but it's good enough as is.
void init_prime_factors() {
  for (uint i = 0; i < primes.size(); i++) {
    for (uint j = 0; j * primes[i] < N+1; j++) {
      prime_factors[j*primes[i]].push_back(primes[i]);
    }
  }
}

uint rad(uint n) {
  uint prod = 1;
  for (const auto p : prime_factors[n]) {
    prod *= p;
  }
  return prod;
}

int main() {
  init_primes();
  init_prime_factors();

  // Build vector of the first N numbers and a parallel vector of
  // their radical values.
  vector<uint> ns;
  vector<uint> rads;
  for (uint n = 1; n <= N; n++) {
    ns.push_back(n);
    rads.push_back(rad(n));
  }

  // Sort by rad(n) or on n if the values are equal.
  sort(ns.begin(), ns.end(), [&rads](uint a, uint b) {
    if (rads[a-1] == rads[b-1]) {
      return a < b;
    } else {
      return rads[a-1] < rads[b-1];
    }
  });

  // Print E(10000).
  cout << ns[9999] << endl;
}
