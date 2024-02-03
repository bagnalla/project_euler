// The simplest / most naive algorithm is more than good enough for
// this one. Runs in about 150ms in my Linux VM. Could be improved but
// there's no reason to bother.

#include <iostream>

using namespace std;

// A number n is "increasing" if n_i ≤ n_(i+1) for all i < ||n||,
// where n_i is the ith digit of n and ||n|| is the total number of
// digits.
bool is_increasing(ulong n) {
  string s = to_string(n);
  for (uint i = 0; i < s.length() - 1; i++) {
    if (s[i] > s[i+1]) {
      return false;
    }
  }
  return true;
}

// A number n is "decreasing" if n_i ≥ n_(i+1) for all i < ||n||.
bool is_decreasing(ulong n) {
  string s = to_string(n);
  for (uint i = 0; i < s.length() - 1; i++) {
    if (s[i] < s[i+1]) {
      return false;
    }
  }
  return true;
}

// A number n is "bouncy" if is it neither increasing nor decreasing.
inline bool is_bouncy(ulong n) {
  return !is_increasing(n) && !is_decreasing(n);
}

int main() {
  uint count = 0;

  // Simply run through the positive integers starting at 0 (we could
  // start somewhere around 100 instead but it doesn't really matter)
  // keeping a running count of the number of bouncy numbers and
  // stopping as soon as the proportion of bouncy numbers reaches the
  // target threshold of 0.99.
  for (ulong n = 0; ; n++) {
    count += is_bouncy(n);
    if (count / (double)n >= 0.99) {
      cout << n << endl;
      break;
    }
  }
}
