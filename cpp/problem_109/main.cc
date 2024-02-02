// Pretty straightforward brute force. It might be hard to generalize
// to numbers of throws greater than 3, but it works quite well for
// the problem as stated. It simply enumerates all possible
// combinations of throw types (e.g., one double [d], a single then a
// double [s, d], two singles then a double [s, s, d], a single then a
// triple then a double [s, t, d], etc.) in such a way that avoids
// duplicates (e.g. if we have [s, d, d] then we don't need [d, s, d])
// and counts for each the number of ways to reach the target.

// The only edge cases are [s, s, d], [d, d, d], and [t, t, d] where
// we have to avoid double counting things like [s1, s3, d1] and [s3, s1, d1].

// We also break early from loops when possible (when the throw values
// exceed the target) to save time, which is sound because the throw
// values are stored in ascending order.

// Runs in under 10ms in my Linux VM.

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

// Map each type of throw to its possible values (singles -> multiples
// of 1, doubles -> multiples of 2, triples -> multiples of 3).
unordered_map<string, vector<uint>> throws;

void init() {
  for (uint i = 1; i <= 20; i++) {
    throws["s"].push_back(i);
  }
  throws["s"].push_back(25);
  for (uint j = 2; j <= 40; j += 2) {
    throws["d"].push_back(j);
  }
  throws["d"].push_back(50);
  for (uint k = 3; k <= 60; k += 3) {
    throws["t"].push_back(k);
  }
}

// Count the number of distinct ways to checkout with a given score.
uint checkouts(uint score) {
  uint count = 0;
  
  // One throw.
  for (const auto d : throws["d"]) {
    if (d > score) {
      break;
    }
    count += d == score;
  }

  // Two throws.
  for (const auto s : vector<string>{"s", "d", "t"}) {
    for (const auto x : throws[s]) {
      if (x > score) {
        break;
      }
      for (const auto d : throws["d"]) {
        if (x + d > score) {
          break;
        }
        count += x + d == score;
      }
    }
  }

  // Three throws.
  for (const auto v : vector<vector<string>>{{"s", "d"}, {"s", "t"}, {"d", "t"},
                                             {"s", "s"}, {"d", "d"}, {"t", "t"}}) {
    for (uint i = 0; i < throws[v[0]].size(); i++) {
      uint x = throws[v[0]][i];
      if (x > score) {
        break;
      }
      // Here we have to explicitly avoid double counting when v[0]
      // and v[1] are the same.
      for (uint j = v[0] == v[1] ? i : 0; j < throws[v[1]].size(); j++) {
        uint y = throws[v[1]][j];
        if (x + y > score) {
          break;
        }
        for (const auto d : throws["d"]) {
          count += x + y + d == score;
        }
      }
    }
  }
  
  return count;
}

int main() {
  init();

  uint sum = 0;
  for (uint n = 2; n < 100; n++) {
    sum += checkouts(n);
  }
  cout << sum << endl;
}
