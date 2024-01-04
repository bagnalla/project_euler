// This solution is based on the insight provided by the PDF document
// attached to problem 69 on the Project Euler website, namely "We see
// that the quotient depends only on the primes dividing n, not on
// their exponents in the prime factorisation of n". I dont fully
// understand the derivation of equation 69.2, although I understand
// 69.1.

// We begin by computing and caching all primes up to N, and then
// computing the distinct prime factors (but not their exponents) for
// all integers up to N. Then by the above insight phi is easy to
// compute so we can brute force search all integers [1<n<N] to find
// the one that minimizes [n/phi(n)].

#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <vector>
using namespace std;

constexpr uint N = 10000000;

// Does d divide k?
constexpr bool divides(int d, int k) {
  return k % d == 0;
}

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

// Compute unique value corresponding to the permutation class of n
// (equivalence class of all permutations of n).
unsigned long long int perm_class(const string& s) {
  unsigned long long int result = 1;
  for (const char c : s) {
    result *= primes[c - '0'];
  }
  return result;
}

bool is_perm(const string& a, const string& b) {
  return perm_class(a) == perm_class(b);
}

int main() {
  cout << "Initializing primes..." << endl;
  init_primes();
  cout << "Initializing prime factors..." << endl;
  init_prime_factors();

  cout << "Running search..." << endl;
  uint min_n = 2;
  double min_n_over_phi_n = numeric_limits<double>::max();  
  for (uint n = 2; n < N; n++) {
    auto phi_n = phi(n);
    auto n_over_phi_n = n / (double)phi_n;
    if (n_over_phi_n < min_n_over_phi_n &&
        is_perm(to_string(n), to_string(phi_n))) {
      // cout << "phi(" << n << ") = " << phi(n) << ", "
      //      << n << " / phi(" << n << ") = " << n / (double)phi(n) << endl;
      min_n_over_phi_n = n_over_phi_n;
      min_n = n;
    }
  }

  cout << min_n << endl;
}
