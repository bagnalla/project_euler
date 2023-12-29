// This solution is much faster than the one in backup.cc.

// The idea is to define a mapping from numbers to unique codes
// corresponding to their permutation equivalence classes (so that two
// numbers map to the same code iff they are permutations of one
// another). Then we simply enumerate cubes from the bottom, keeping
// track of how many times we've encountered elements of each
// permutation class. We stop when we've seen 5 elements of a
// permutation class (after checking that the other permutations are
// not cubes, to confirm that there are exactly 5 cubes in the class).

#include <cmath>
#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

static vector<uint> primes{2, 3, 5, 7, 11, 13, 17, 19, 23, 29};

// Compute unique value corresponding to the permutation class of n
// (equivalence class of all permutations of n). Based on prime factorization.
unsigned long long int perm_class(unsigned long long int n) {
  unsigned long long int result = 1;
  const string s = to_string(n);
  for (const char c : s) {
    result *= primes[c - '0'];
  }
  return result;
}

int main() {
  unordered_map<unsigned long long int, vector<unsigned long long int>> history;
  unordered_map<unsigned long long int, uint> counts;

  // Enumerate cubes...
  for (unsigned long long int i = 1;; i++) {
    const unsigned long long int n = i*i*i;

    // Get permutation class of the cube.
    const auto pc = perm_class(n);
    history[pc].push_back(n);
    counts[pc]++;

    // If we've found 5 cubes belonging to this permutation class, we
    // may have found a solution.
    if (counts[pc] == 5) {
      // Need to check that the count is exactly 5.
      // Keep enumerating cubes...
      while (i++) {
        const unsigned long long int m = i*i*i;
        // Can stop searching when the number of digits increases (no
        // more permutations past that point).
        if (log10(m) > log10(n)) {
          break;
        }
        // If we find another permutation of n then the total number
        // of permutations is *greater* than 5 so we haven't actually
        // found a solution and must continue searching.
        if (perm_class(m) == pc) {
          goto next;
        }
      }

      // We done.
      for (const auto m : history[pc]) {
        cout << m << endl;
      }
      break;
    next: {}
    }
  }
}
