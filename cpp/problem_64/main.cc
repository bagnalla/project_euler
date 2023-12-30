// Solution based on the algorithm given here:
// https://math.stackexchange.com/a/4425617.

#include <cmath>
#include <iostream>
#include <tuple>
#include <vector>
using namespace std;

uint period_of_sqrt(uint n) {
  double k = sqrt(n);
  vector<tuple<uint, uint, uint>> seen;
  uint a0 = (uint)k;

  // Exit early if sqrt(n) is an integer (and thus n is a perfect square).
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
      // Search for beginning of cycle.
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
  uint count = 0;
  for (uint i = 1; i <= 10000; i++) {
    count += period_of_sqrt(i) % 2 == 1;
  }
  cout << count << endl;
}
