// Recursive brute-force with memoization. The code is a little
// complicated but the basic idea is to define by recursion a function
// that takes a set of digits and produces all the distinct sets of
// primes formed from using each digit exactly once:

// Given a set of digits 'digits',
// Base cases:

// * If the set is empty, return {{}} (the singleton set containing
// * the empty set).

// * If the set contains a single digit n, return {{n}} if n is prime,
// * else {} (the empty set).

// Recursive case: For each subset 's' of 'digits':

// 1) Find all the primes that can be formed from 's'.
// 2) Recursively compute the sets of primes for 'digits \ 's'.
// 3) Return the cartesian product of the sets obtained from steps 1
// and 2 (treating each prime in the set from step 1 as a singleton
// set) with duplicates removed.

#include <bitset>
#include <cmath>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;

bool is_prime(uint n) {
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

// Generate all non-empty subsets of set [s].
template <typename T>
vector<vector<T>> subsets(const vector<T> &s) {
  vector<vector<T>> result;
  for (uint i = 1; i < pow(2, s.size()); i++) {
    vector<T> subset;
    for (uint j = 0; j < s.size(); j++) {
      if (i & 1 << j) {
        subset.push_back(s[j]);
      }
    }
    result.push_back(subset);
  }
  return result;
}

template <typename T>
constexpr bool contains(const vector<T> &v, const T &x) {
  return find(v.begin(), v.end(), x) != v.end();
}

// Naive set difference for sets represented via vectors. Fast for small sets.
template <typename T>
vector<T> set_minus(const vector<T> &a, const vector<T> &b) {
  vector<T> diff;
  for (const auto x : a) {
    if (!contains(b, x)) {
      diff.push_back(x);
    }
  }
  return diff;
}

// Compute the number corresponding to a vector of digits.
uint num_from_digits(const vector<uint> &digits) {
  uint sum = 0;
  uint p = 0;
  for (const auto d : digits) {
    sum += d * pow(10, p++);
  }
  return sum;
}

// Find all the primes that can be formed by using each digit in the
// given vector exactly once. Memoized.
vector<uint> digits_primes(vector<uint> digits) {
  static unordered_map<bitset<9>, vector<uint>> cache;
  bitset<9> bs;
  for (const auto d : digits) {
    bs[d-1] = true;
  }  
  if (cache.contains(bs)) {
    return cache.at(bs);
  }

  vector<uint> primes;

  do {
    const auto p = num_from_digits(digits);
    if (is_prime(p)) {
      primes.push_back(p);
    }
  } while (next_permutation(digits.begin(), digits.end()));

  cache[bs] = primes;
  return primes;
}

// Function object for hashing vectors of uints.
struct VectorHasher {
  size_t operator()(vector<uint> const& vec) const {
    size_t seed = vec.size();
    for(auto x : vec) {
      x = ((x >> 16) ^ x) * 0x45d9f3b;
      x = ((x >> 16) ^ x) * 0x45d9f3b;
      x = (x >> 16) ^ x;
      seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

// Remove duplicates from a vector.
template <typename T>
vector<vector<T>> dedup(const vector<vector<T>> &vs) {
  unordered_set<vector<T>, VectorHasher> s;
  for (const auto v : vs) {
    s.insert(v);
  }
  return vector<vector<T>>(s.begin(), s.end());
}

// The main recursive function described in the comments at the top of
// the file.
vector<vector<uint>> prime_sets(const vector<uint> &digits) {
  static unordered_map<bitset<9>, vector<vector<uint>>> cache;
  bitset<9> bs;
  for (const auto d : digits) {
    bs[d-1] = true;
  }
  if (cache.contains(bs)) {
    return cache.at(bs);
  }

  if (digits.size() == 0) {
    return {{}};
  }
  if (digits.size() == 1) {
    if (is_prime(digits[0])) {
      return {{digits[0]}};
    } else {
      return {};
    }
  }

  vector<vector<uint>> ps;

  for (const auto subset : subsets(digits)) {
    const auto primes = digits_primes(subset);
    auto rest = prime_sets(set_minus(digits, subset));
    for (const auto s : rest) {
      for (const auto p : primes) {
        vector<uint> v(s);
        v.push_back(p);
        sort(v.begin(), v.end());
        ps.push_back(v);
      }
    }
  }

  ps = dedup(ps);
  cache[bs] = ps;
  return ps;
}

int main() {
  const auto sets = prime_sets({1, 2, 3, 4, 5, 6, 7, 8, 9});
  cout << sets.size() << endl;
}
