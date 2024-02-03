// The code for this one is confusing but very fast and generalized to
// work for any S(n, d). Runs in about 33ms in my Linux VM. The idea
// is fairly simple:

// Say we want to compute S(10, 1), i.e., the sum of the 10-digit
// prime numbers with the maximum amount of repeated 1s. First, we'll
// simply guess that the number of repeated 1s is nine, and try all
// possibilities to see how many of them are prime:

// 1211111111
// 1311111111
// 1411111111
// ...
// 1121111111
// 1131111111
// ...
// 1111111118
// 1111111119

// There are 89 such possibilities, and 11 of them are prime. So, we
// were right in our guess that the maximum number of repeated 1s was
// 9, and we can add these primes to our running sum and move on to
// computing S(10, 2).

// If we find no primes from our initial guess, then we decrease our
// guess by one and try again. This turns out to be necessary for
// S(10, 2), where the maximum number of repeated 2s in a prime is 8
// (and there are 39 such primes).

// That's basically it, but the code for generating all the
// possibilities is a little complicated. Only M(10, 0), M(10, 2) and
// M(10, 8) are equal to 8, and the rest (M(10, d) for d ∉ {0, 2, 8})
// are equal to 9.

#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

// N = 4 for the example in the problem statement.
constexpr uint N = 10;

constexpr bool isPrime(ulong n) {
  if (n % 2 == 0) {
    return n == 2;
  }
  for (ulong i = 3; i*i <= n; i += 2) {
    if (n % i == 0) {
      return false;
    }
  }
  return n > 1;
}

// Compute all sets of n distinct indices where [lb <= ix < ub] for
// each index.
vector<vector<uint>> indices(uint lb, uint ub, uint n) {
  vector<vector<uint>> ixs;

  if (n <= 1) {
    for (uint i = lb; i < ub; i++) {
      ixs.push_back({i});
    }
  } else {
    for (uint i = lb; i < ub; i++) {
      vector<vector<uint>> vs = indices(i+1, ub, n-1);
      for (auto &v : vs) {
        v.push_back(i);
      }
      ixs.insert(ixs.end(), vs.begin(), vs.end());
    }
  }

  return ixs;
}

// Given an initial string [s] (e.g., "1111111111", "2222222222") and
// set of indices [ixs], try replacing characters in [s] at indices in
// ixs[j] for [j >= i] with all possible digits and produce a vector
// of all the primes found from this process.
vector<ulong> find_primes_with_ixs_replaced(string &s, const vector<uint> &ixs, uint i) {
  if (i == ixs.size()) {
    ulong n = stoull(s);
    return isPrime(n) ? vector<ulong>{n} : vector<ulong>{};
  }

  vector<ulong> primes;
  char c = s[ixs[i]];
  for (uint d = ixs[i] == 0 ? '1' : '0'; d <= '9'; d++) {
    if (d != c) {
      s[ixs[i]] = d;
      auto ps = find_primes_with_ixs_replaced(s, ixs, i+1);
      primes.insert(primes.end(), ps.begin(), ps.end());
    }
  }
  s[ixs[i]] = c;

  return primes;
}

// Find all primes obtained from starting with a string of all
// [digit]s (e.g., "1111111111" when N=10 and digit='1') and replacing
// characters at indices specified by the sets of indices in [ixss].
vector<ulong> find_primes(char digit, const vector<vector<uint>> &ixss) {
  string s(N, digit);

  vector<ulong> primes;
  for (const auto ixs : ixss) {
    auto ps = find_primes_with_ixs_replaced(s, ixs, 0);
    primes.insert(primes.end(), ps.begin(), ps.end());
  }

  return primes;
}

int main() {
  ulong sum = 0;

  // Treat zero as a special case since the leftmost digit will always
  // have to be replaced.
  vector<vector<uint>> zero_indices;
  // We know from experimenting that M(10, 0) is 8, and we also know
  // that index 0 will always have to be replaced, so we just
  // calculate the indices as if M(10, 0) were 9 (i.e., only one digit
  // needs to be replaced) and add the index 0 to each of the
  // resulting sets of indices (which are singletons).
  for (auto ixs : indices(1, N, 1)) {
    ixs.push_back(0);
    zero_indices.push_back(ixs);
  }
  auto primes = find_primes('0', zero_indices);
  for (const auto p : primes) {
    sum += p;
  }

  // General code for the rest.
  for (char d = '1'; d <= '9'; d++) {
    for (uint i = 1; ; i++) {
      auto ixss = indices(0, N, i);
      primes = find_primes(d, ixss);
      if (!primes.empty()) {
        for (const auto p : primes) {
          sum += p;
        }
        break;
      }
    }
  }

  cout << sum << endl;
}
