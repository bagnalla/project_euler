// Relatively straightforward brute force recursive
// algorithm. Computes the answer in under 6 seconds in my Linux VM.

#include <cmath>
#include <functional>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

// This computes the sum expressions as vectors of integers. Useful
// for debugging the algorithm.
vector<vector<uint>> f(uint n) {
  // Compute ways to sum n with integers <= ub.
  function<vector<vector<uint>>(uint, uint)> go = [&go](uint n, uint ub){
    vector<vector<uint>> result;
    if (n <= ub) {
      result.push_back({n});
    }
    for (uint i = 1; i <= min(n-1, ub); i++) {
      auto vs = go(n-i, i);
      for (auto& v : vs) {
        v.insert(v.begin(), i);
        // v.push_back(i);
        result.push_back(v);
      }
    }
    return result;
  };
  return go(n, n);
}

// This is a translation of the above to just count the number of sum
// expressions instead of actually constructing them.
uint g(uint n) {
  // Compute number of ways to sum n with integers <= ub.
  function<uint(uint, uint)> go = [&go](uint n, uint ub){
    uint sum = n <= ub;
    for (uint i = 1; i <= min(n-1, ub); i++) {
      sum += go(n-i, i);
    }
    return sum;
  };
  return go(n, n);
}

// The three functions below implement a faster algorithm based on the
// recurrence relation described here:
// https://en.wikipedia.org/wiki/Pentagonal_number_theorem.
constexpr ulong penta(int n) {
  return (3 * n*n - n) / 2;
}

constexpr int alternating(uint n) {
  return pow(-1, n+1) * ((n+1) / 2);
}

// Memoized partition function.
ulong p(uint n) {
  if (n == 0) {
    return 1;
  }
  if (n < 0) {
    return 0;
  }

  static unordered_map<uint, ulong> cache;
  if (cache.contains(n)) {
    return cache[n];
  }
  
  uint i = 1;
  int k = alternating(i);
  ulong g_k = penta(k);
  ulong sum = 0;
  while (n >= g_k) {
    sum += pow(-1, k-1) * p(n - g_k);
    k = alternating(++i);
    g_k = penta(k);
  }
  
  cache[n] = sum;
  return sum;
}

int main() {
  // auto vs = f(100);
  // cout << vs.size()-1 << endl;

  // auto sum = g(100);
  // cout << sum-1 << endl;

  // for (uint i = 0; i < 100; i++) {
  //   cout << p(i) << endl;
  // }
  cout << p(100)-1 << endl;
}
