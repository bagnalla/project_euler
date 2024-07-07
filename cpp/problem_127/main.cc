// I was stuck on this one for a while because I didn't realize the
// radical product calculation was overflowing.

// Anyway, it's a pretty straightforward brute force. Most of the
// speedup comes from precomputing the radicals. We also exploit the
// fact that rad(a*b*c) = rad(a)*rad(b)*rad(c) when a, b, and c are
// coprime.

// Runs in about 2.5s in my Linux VM.

#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

constexpr uint N = 120000;

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

vector<uint> primes;

void init_primes() {
  primes.push_back(2);
  for (uint i = 3; i < N; i += 2) {
    if (isPrime(i)) {
      primes.push_back(i);
    }
  }
}

vector<vector<uint>> prime_factors(N+1, vector<uint>{});

void init_prime_factors() {
  for (uint i = 0; i < primes.size(); i++) {
    for (uint j = 0; j * primes[i] < N; j++) {
      prime_factors[j*primes[i]].push_back(primes[i]);
    }
  }
}

ulong rad(uint n) {
  ulong prod = 1;
  for (const auto p : prime_factors[n]) {
    prod *= p;
  }
  return prod;
}

vector<ulong> rads;

void init_rads() {
  for (uint i = 0; i < N; i++) {
    rads.push_back(rad(i));
  }
}

// Euclidean algorithm for greatest common divisor.
constexpr uint gcd(uint a, uint b) {
  while (b != 0) {
    a = a % b;
    swap(a, b);
  }
  return a;
}

int main() {
  init_primes();
  init_prime_factors();
  init_rads();

  ulong sum = 0;
  
  for (uint c = 3; c < N; c++) {
    for (uint b = c / 2 + 1; b < c; b++) {
      uint a = c - b;
      if (rads[a] * rads[b] * rads[c] < c && gcd(a, c) == 1) {
        sum += c;
      }
    }
  }
  
  cout << sum << endl;
}
