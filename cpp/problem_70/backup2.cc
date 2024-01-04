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
  if (n == 2) {
    return true;
  }
  for (uint i = 3; i*i <= n; i += 2) {
    if (n % i == 0) {
      return false;
    }
  }
  return true;
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

// static vector<vector<uint>> prime_factors(N, vector<uint>{});
static vector<set<uint>> prime_factors(N, set<uint>{});

void init_prime_factors() {
  for (uint i = 0; i < primes.size(); i++) {
    for (uint j = 1; j * primes[i] < N; j++) {
      prime_factors[j*primes[i]].insert(primes[i]);
    // for (uint j = 0; primes[j] * primes[i] < N; j++) {
    //   prime_factors[primes[j]*primes[i]].insert(primes[i]);
    }
  }
}

uint phi(uint n) {
  double result = n;
  // for (uint i = 0; i < primes.size() && primes[i] <= n; i++) {
  //   if (n % primes[i] == 0) {
  //     result *= 1 - 1.0 / primes[i];
  //   }
  // }
  
  // cout << n << ": ";
  for (const auto p : prime_factors[n]) {
    // cout << p << " ";
    result *= 1 - 1.0 / p;
  }
  // cout << endl;
  
  return result;
}

// Compute unique value corresponding to the permutation class of n
// (equivalence class of all permutations of n). Based on prime factorization.
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
  init_primes();
  init_prime_factors();

  // cout << primes.size() << endl;

  // for (const auto p : primes) {
  //   cout << p << endl;
  // }

  uint min_n = 2;
  double min_n_over_phi_n = numeric_limits<double>::max();  
  for (uint n = 2; n < N; n++) {
    auto phi_n = phi(n);
    auto n_over_phi_n = n / (double)phi_n;
    // cout << n_over_phi_n << endl;
    if (n_over_phi_n < min_n_over_phi_n &&
        is_perm(to_string(n), to_string(phi_n))) {
      cout << "phi(" << n << ") = " << phi(n) << ", "
           << n << " / phi(" << n << ") = " << n / (double)phi(n) << endl;
    min_n_over_phi_n = n_over_phi_n;
    min_n = n;
    }
  }

  cout << min_n << endl;
  // for (const auto p : prime_factors[min_n]) {
  //   cout << p << " ";
  // }
  // cout << endl;
}
