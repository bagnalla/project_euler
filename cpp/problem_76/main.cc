// Relatively straightforward brute force recursive
// algorithm. Computes the answer in under 6 seconds in my Linux VM.

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

int main() {
  // auto vs = f(90);
  // cout << vs.size()-1 << endl;

  auto sum = g(100);
  cout << sum-1 << endl;
}
