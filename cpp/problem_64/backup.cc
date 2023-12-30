#include <cmath>
#include <iostream>
// #include <unordered_set>
// #include <utility>
#include <tuple>
#include <vector>
using namespace std;

// inline __float128 abs(__float128 x) {
//   return x < 0 ? -x : x;
// }

// inline bool eq(const pair<uint, __float128>& a,
//                const pair<uint, __float128>& b) {
//   return a.first == b.first && abs(a.second - b.second) <= 0.00001;
// }

// bool contains(const vector<pair<uint, __float128>>& v,
//               const pair<uint, __float128>& x) {
//   for (const auto p : v) {
//     if (eq(p, x)) {
//       return true;
//     }
//   }
//   return false;
// }

// uint period_of_sqrt(uint n) {
//   __float128 k = sqrt(n);
//   // unordered_set<pair<uint, double>, hashFunction> seen;
//   vector<pair<uint, __float128>> seen;
  
//   for (size_t i = 0;; i++) {
//     const auto digit = (uint)k;
//     // cout << digit << endl;
//     // cout << k << endl;
//     if (k == digit) {
//       return i;
//     }
//     // k = 1 / (k - digit);
//     k = (k + digit) / (k*k - digit*digit);
//     if (contains(seen, {digit, k})) {
//       for (size_t j = 0; j < seen.size(); j++) {
//         if (eq(seen[j], {digit, k})) {
//           return i - j;
//         }
//       }
//       // return i - 1;
//     } else {
//       seen.push_back(pair{digit, k});
//     }
//   }
// }

// https://math.stackexchange.com/a/4425617

uint period_of_sqrt(uint n) {
  double k = sqrt(n);
  vector<tuple<uint, uint, uint>> seen;
  uint a0 = (uint)k;

  if (a0 == k) {
    return 0;
  }
  
  uint a = a0;
  uint r = 0;
  uint s = 1;

  seen.push_back({a, r, s});
  
  for (size_t i = 0;; i++) {
    a = (uint)((r + a0) / s);
    r = a * s - r;
    s = (n - r*r) / s;

    if (find(seen.cbegin(), seen.cend(), tuple{a, r, s}) != seen.cend()) {
      for (size_t j = 0; j < seen.size(); j++) {
        if (seen[j] == tuple{a, r, s}) {
          return i - j + 1;
        }
      }
    } else {
      seen.push_back({a, r, s});
    }
  }
}

int main() {
  
  // cout << period_of_sqrt(2) << endl;

  uint count = 0;
  for (uint i = 1; i <= 10000; i++) {
    count += period_of_sqrt(i) % 2 == 1;
  }
  cout << count << endl;
  
}
