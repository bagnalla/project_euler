// This one blew my mind a little (not the only one to do so). I first
// found the solution by implementing a naive brute force algorithm
// ('f' below) and noticing that the delta between each f(n) and
// f(n-1) was equal to phi(n), so since we have an efficient way of
// computing phi from previous problems we can obtain the answer by
// simply summing phi(n) over all 1<n<1000000.

// I didn't understand at first why that was the case, but then I realized:

// 1) phi(n) is exactly the number of positive reduced fractions with
// denominator equal to n, and

// 2) the farey sequence (the ascending sequence of reduced fractions
// with denominators up to n) considered as a set (since we only care
// about the number of elements) is monotonic in n, meaning that
// [farey(n) ⊆ farey(n+1)] for all n (equivalently [farey(n) ⊆ farey(m)
// for all n ≤ m]). More precisely, [farey(n) = farey(n-1) ∪ {i ∣ 0 < i
// and gcd(i, n) = 1}], which means that [|farey(n)| = |farey(n-1) +
// phi(n)|] (since the union is disjoint). It follows from a simple
// induction that [|farey(n)| = ∑phi(n)].

#include <cmath>
#include <iostream>
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

    count++;

    a = c;
    b = d;
    c = p;
    d = q;
  }

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

static vector<vector<uint>> prime_factors(N, vector<uint>{});

// This could probably be optimized but it's good enough as is.
void init_prime_factors() {
  for (uint i = 0; i < primes.size(); i++) {
    for (uint j = 0; j * primes[i] < N; j++) {
      prime_factors[j*primes[i]].push_back(primes[i]);
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
  }
  cout << sum << endl;
}
