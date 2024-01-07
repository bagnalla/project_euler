// Relatively straightforward brute force recursive
// algorithm. Computes the answer instantly.

#include <cmath>
#include <functional>
#include <iostream>
#include <set>
#include <vector>
using namespace std;

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

// // This computes the sum expressions as vectors of integers. Useful
// // for debugging the algorithm.
// vector<vector<uint>> f(uint n) {
//   // Compute ways to sum n with integers <= ub.
//   function<vector<vector<uint>>(uint, uint)> go = [&go](uint n, uint ub){
//     vector<vector<uint>> result;
//     if (n <= ub && isPrime(n)) {
//       result.push_back({n});
//     }
//     for (uint i = 1; i <= min(n-1, ub); i++) {
//       if (isPrime(i)) {
//         auto vs = go(n-i, i);
//         for (auto& v : vs) {
//           v.insert(v.begin(), i);
//           // v.push_back(i);
//           result.push_back(v);
//         }
//       }
//     }
//     return result;
//   };
//   return go(n, n);
// }

// This is a translation of the above to just count the number of sum
// expressions instead of actually constructing them.
uint f(uint n) {
  // Compute ways to sum n with integers <= ub.
  function<uint(uint, uint)> go =
    [&go](uint n, uint ub){
      uint sum = 0;
      if (n <= ub && isPrime(n)) {
        sum++;
      }
      for (uint i = 1; i <= min(n-1, ub); i++) {
        if (isPrime(i)) {
          sum += go(n-i, i);
        }
      }
      return sum;
    };
  return go(n, n);
}


int main() {
  for (uint i = 0;; i++) {
    if (f(i) >= 5000) {
      cout << i << endl;
      break;
    }
  }
}
